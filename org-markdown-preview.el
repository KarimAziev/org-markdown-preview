;;; org-markdown-preview.el --- A minor mode for preview markdown in org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-markdown-preview
;; Version: 0.1.0
;; Keywords: outlines convenience docs
;; Package-Requires: ((emacs "27.1") (websocket "1.15") (simple-httpd "1.5.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for preview markdown in org mode.

;;; Commands

;; M-x  `org-markdown-preview-mode'
;;       A minor mode that export org file to markdown on save and launch server for live preview.

;; M-x `org-markdown-preview-markdown-write'
;;      Write `org-markdown-preview-md-content' to the markdown file.
;;      Th file name based on the local value of `org-markdown-preview-preview-buffer'.

;; M-x `org-markdown-preview-websocket-send-html'
;;      Write org content to markdown file and refresh websocket clients.
;;      Org content is taken from `org-markdown-preview-preview-buffer'.

;; M-x `org-markdown-preview-browse-preview'
;;      Visit a served page in a browser.
;;      Uses `browse-url' to launch a browser

;;; Customization

;; `org-markdown-preview-pandoc-output-type'
;;      Markdown output type for `pandoc'

;;; Code:

(require 'websocket)
(require 'simple-httpd)

(declare-function ghub-post "ghub")

(defvar org-markdown-preview-websockets nil)
(defvar org-markdown-preview-websocket-server nil)
(defvar org-markdown-preview-markdown-current-html nil)

(defconst org-markdown-preview-data-root
  (file-name-directory (if (bound-and-true-p load-file-name) load-file-name
                         (buffer-file-name)))
  "The directory where `org-markdown-preview' package exists.")

(defcustom org-markdown-preview-pandoc-output-type "gfm"
  "Markdown output type for `pandoc'."
  :type '(choice :tag "Pandoc output type"
                 (string :tag "GitHub-Flavored Markdown" "gfm")
                 (string :tag "Pandoc’s Markdown" "markdown")
                 (string :tag "MultiMarkdown" "markdown_mmd")
                 (string :tag "PHP Markdown Extra" "markdown_phpextra")
                 (string :tag "original unextended Markdown" "markdown_strict")
                 (string :tag "Other"))
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-use-github-api t
  "Whether to use `github' API when rendering HTML content for preview.

Requires `ghub' to be installed."
  :type 'boolean
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-scroll-delay 0.5
  "How many seconds to wait after last command before scroll sync.
If nil, inhibit scroll sync at all."
  :group 'org-markdown-preview
  :type '(radio  (const
                  :tag "None (disable scroll)"
                  :value nil)
                 (number
                  :tag "Seconds")))

(defcustom org-markdown-preview-pandoc-options '("--quiet"
                                                 "--embed-resources"
                                                 "--standalone"
                                                 "--highlight-style=zenburn"
                                                 "--wrap=none")
  "Extra pandoc options."
  :group 'org-markdown-preview
  :type '(repeat string))

(defcustom org-markdown-preview-refresh-behavior 'after-save-hook
  "When to refresh preview page.
It should be a name of a hook (a symbol) where should refresh handler be added."
  :group 'org-markdown-preview
  :type '(radio  (const
                  :tag "After Save Hook"
                  :value after-save-hook)
                 (const
                  :tag "Post Insert Hook"
                  :value post-self-insert-hook)
                 (symbol
                  :tag "Other")))

(defcustom org-markdown-preview-refresh-delay 0.5
  "How many seconds to wait after running hooks before refreshing content.
If nil, refresh after imediatelly after running
`org-markdown-preview-refresh-behavior'"
  :group 'org-markdown-preview
  :type '(radio  (const
                  :tag "Immediately"
                  :value nil)
                 (number
                  :tag "Seconds")))


(defcustom org-markdown-preview-browse-fn (if (and window-system
                                                   (featurep 'xwidget-internal))
                                              'org-markdown-preview-browse-with-xwidget
                                            'browse-url)
  "Function for browsing preview page.

It will be called with one argument - url to open.

Default value is to use xwidgets if available, othervise `browse-url'."
  :type '(radio  (function-item org-markdown-preview-browse-with-xwidget)
                 (function-item browse-url)
                 (function
                  :tag "Custom function"))
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-websocket-port 7071
  "Websocket port to use."
  :type 'integer
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-preprocess-org-content-hook '(org-markdown-preview-preprocess-org-content)
  "Hook for preprocessing Org content before Markdown preview conversion.

Each function in the hook should accept no arguments and will be called in the
temporary buffer prior to its processing by Pandoc."
  :group 'org-markdown-preview
  :type 'hook)

(defcustom org-markdown-preview-post-process-org-content-hook '(org-markdown-preview-strip-propererties)
  "Hook for post processing Org content before Markdown preview conversion.

Each function in the hook should accept no arguments and will be called in the
temporary buffer prior to its processing by Pandoc."
  :group 'org-markdown-preview
  :type 'hook)


(defcustom org-markdown-preview-post-process-md-content-hook '(org-markdown-preview-cleanup-escape-sequences)
  "Hook for post processing markdown content before Markdown preview conversion.

Each function in the hook should accept no arguments and will be called in the
temporary buffer prior to its processing by Pandoc."
  :group 'org-markdown-preview
  :type 'hook)

(defun org-markdown-preview-get-url ()
  "Return an url with served page."
  (let* ((proc (get-process "httpd"))
         (proc-info (process-contact proc t))
         (raw-host (plist-get proc-info :host))
         (host (if (member raw-host
                           '(nil local "127.0.0.1" "::1" "0.0.0.0" "::"))
                   "localhost"
                 raw-host))
         (local-addr (plist-get proc-info :local))
         (port (aref local-addr (1- (length local-addr))))
         (path (if org-markdown-preview-use-github-api
                   "org-markdown-preview-ghub"
                 "org-markdown-preview")))
    (format "http://%s:%d/%s"
            host port path)))

(defun org-markdown-preview-browse-with-xwidget (url)
  "Visit an URL in xwidget in other window."
  (require 'xwidget)
  (let ((orig-wind (selected-window)))
    (with-selected-window
        (if (minibuffer-window-active-p orig-wind)
            (with-minibuffer-selected-window
              (let ((wind (selected-window)))
                (or
                 (window-right wind)
                 (window-left wind)
                 (split-window-right))))
          (let ((wind (selected-window)))
            (or
             (window-right wind)
             (window-left wind)
             (split-window-right))))
      (xwidget-webkit-browse-url url))))

;;;###autoload
(defun org-markdown-preview-browse-preview ()
  "Visit a served page in a browser.
Uses `browse-url' to launch a browser"
  (interactive)
  (if-let* ((url (org-markdown-preview-get-url)))
      (funcall org-markdown-preview-browse-fn url)
    (user-error
     "org-markdown-preview: Couldn't resolve url, ensure that httpd is running")))

(defun org-markdown-preview--websocket-send-msg-to-client (type &optional
                                                                payload)
  "Notify all opened sockets with message TYPE and PAYLOAD."
  (setq org-markdown-preview-websockets
        (seq-filter #'websocket-openp org-markdown-preview-websockets))
  (dolist (socket org-markdown-preview-websockets)
    (when (and socket type)
      (websocket-send-text
       socket
       (json-encode
        `(("type" . ,type)
          ("payload" . ,payload)))))))

(defvar org-markdown-preview-preview-buffer nil)
(defvar org-markdown-preview-md-content nil)

(defun org-markdown-preview-strip-propererties ()
  "Remove properties from `org-mode' markdown preview."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward
              ":PROPERTIES:[\n]+[\s\t]+:CUSTOM_ID:[\s\t][^\n]+[\n]+[\s\t]+:END:[\n]"
              nil t
              1)
        (replace-match "")))))


(defun org-markdown-preview-cleanup-escape-sequences ()
  "Clean up escape sequences like \\[X\\] or \\[\\!TIP\\] in the current buffer.

For example, transforms \\[X\\] to [X], \\[\\!TIP\\] into [!TIP] and so on."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\[\\([^\\]*\\)\\\\\\]" nil t)
      (replace-match "[\\1]" nil nil))))

(defun org-markdown-preview-preprocess-org-content ()
  "Replace `emacs-lisp' with `elisp' in \"#+begin_src\" tags.
The reason is Pandoc will convert `emacs-lisp' to `commonlisp'
in the markdown output."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "#\\+begin_src[\s]+\\_<\\(emacs-lisp\\)\\_>" nil
                                t
                                1)
        (replace-match "elisp" nil nil nil 1)))))

(defun org-markdown-preview-pandoc-from-string (string input-type output-type)
  "Execute `pandoc' on STRING in INPUT-TYPE to OUTPUT-TYPE."
  (let ((args (append
               (list "pandoc" t t nil)
               (list "-f" input-type "-t"
                     output-type)
               org-markdown-preview-pandoc-options)))
    (princ args)
    (with-temp-buffer
      (insert string)
      (pcase input-type
        ("org" (run-hooks 'org-markdown-preview-preprocess-org-content-hook)))
      (let ((status
             (apply #'call-process-region (append (list (point-min)
                                                        (point-max))
                                                  args))))
        (when (and (numberp status)
                   (zerop status))
          (pcase output-type
            ("org" (run-hooks
                    'org-markdown-preview-post-process-org-content-hook))
            ((or "gfm"
                 "markdown"
                 "markdown_mmd"
                 "markdown_phpextra"
                 "markdown_strict"
                 "markdown_github")
             (run-hooks 'org-markdown-preview-post-process-md-content-hook))))
        (buffer-string)))))



(defun org-markdown-preview--refresh-buffer ()
  "Convert and send current buffer's content to preview page."
  (cond ((or (derived-mode-p 'org-mode)
             (and buffer-file-name
                  (equal "org" (file-name-extension
                                buffer-file-name))))
         (setq org-markdown-preview-md-content
               (org-markdown-preview-pandoc-from-string
                (buffer-substring-no-properties
                 (point-min)
                 (point-max))
                "org"
                org-markdown-preview-pandoc-output-type)))
        (t
         (setq org-markdown-preview-md-content
               (let ((content (buffer-substring-no-properties
                               (point-min)
                               (point-max))))
                 (if org-markdown-preview-post-process-md-content-hook
                     (with-temp-buffer
                       (insert content)
                       (run-hooks
                        'org-markdown-preview-post-process-md-content-hook)
                       (buffer-string))
                   content))))))


(defun org-markdown-preview--str-replace (old new s)
  "Replace occurrences of OLD with NEW in string S.

Argument OLD is the substring to be replaced.

Argument NEW is the replacement substring.

Argument S is the original string where replacements will occur."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun org-markdown-preview-add-ids-to-links (html-str)
  "Add unique id attributes to heading tags (h1-h6) in the given HTML-STR string."
  (let ((pos 0)
        (case-fold-search t) ;; Make searches case-insensitive
        (counter (make-hash-table :test 'equal))
        (dash-protection-sym
         "09876543214b825dc642cb6eb9a060e54bf8d69288fbee49041234567890")
        (underscore-protection-symbol
         "afec96cafb7bc4b0e216bfe86db4bd6c4aab44bca19dd9999b11e162f595d711"))
    (while (string-match "<\\(h[1-6]\\)\\([^>]*\\)>\\([^<]+\\)</\\1>" html-str
                         pos)
      (let* ((tag (match-string 1 html-str)) ;; The tag name (e.g., h1, h2)
             (attrs (match-string 2 html-str)) ;; Any existing attributes in the tag
             (text (match-string 3 html-str)) ;; The text inside the heading
             (id
              (thread-last text
                           string-trim
                           downcase
                           (org-markdown-preview--str-replace
                            "-"
                            dash-protection-sym)
                           (org-markdown-preview--str-replace
                            "_"
                            underscore-protection-symbol)
                           (replace-regexp-in-string
                            "[[:punct:]]" "")
                           (org-markdown-preview--str-replace
                            dash-protection-sym
                            "-")
                           (org-markdown-preview--str-replace
                            underscore-protection-symbol
                            "_")
                           (org-markdown-preview--str-replace " " "-")))
             (base-id id)
             (dup-count 0))
        ;; Ensure the ID is unique by incrementing a counter if necessary
        (while (gethash id counter)
          (setq dup-count (1+ dup-count))
          (setq id (format "%s-%d" base-id dup-count)))
        ;; Add the ID to the tracking hash table
        (puthash id t counter)
        ;; Replace the heading with the new ID
        (setq html-str
              (replace-match
               (format "<%s id=\"%s\"%s>%s</%s>" tag id attrs text tag)
               t t html-str))
        ;; Move the search forward
        (setq pos (match-end 0))))
    html-str))

;;;###autoload
(defun org-markdown-preview-websocket-send-html ()
  "Write org content to markdown file and refresh websocket clients.
Org content is taken from `org-markdown-preview-preview-buffer'."
  (interactive)
  (with-current-buffer org-markdown-preview-preview-buffer
    (org-markdown-preview--refresh-buffer)
    (when org-markdown-preview-md-content
      (setq org-markdown-preview-markdown-current-html
            (org-markdown-preview-pandoc-from-string
             org-markdown-preview-md-content
             org-markdown-preview-pandoc-output-type
             "html5"))
      (org-markdown-preview--websocket-send-msg-to-client
       "html"
       org-markdown-preview-markdown-current-html))))

;;;###autoload
(defun org-markdown-preview-websocket-send-ghub-html ()
  "Send the refreshed Markdown content to the client via WebSocket."
  (interactive)
  (with-current-buffer org-markdown-preview-preview-buffer
    (org-markdown-preview--refresh-buffer)
    (when org-markdown-preview-md-content
      (org-markdown-preview--ghub-md-to-html
       org-markdown-preview-md-content
       (lambda (value)
         (setq org-markdown-preview-markdown-current-html
               (org-markdown-preview-add-ids-to-links value))
         (org-markdown-preview--websocket-send-msg-to-client
          "html"
          org-markdown-preview-markdown-current-html))))))

(defun org-markdown-preview-refresh-buffer ()
  "Org-Markdown-Preview."
  (if org-markdown-preview-use-github-api
      (org-markdown-preview-websocket-send-ghub-html)
    (org-markdown-preview-websocket-send-html)))

(defun org-markdown-preview-calc-size-percent ()
  "Calulate scroll percent relative to Emacs buffer.
The result is a floating number from 0 to 1, formatted to string."
  (let ((value (/ (float (truncate (* 100 (/ (float (-  (line-number-at-pos)
                                                        (/
                                                         (count-screen-lines
                                                          (window-start)
                                                          (point))
                                                         2)))
                                             (count-lines (point-min)
                                                          (point-max))))))
                  100)))
    (number-to-string value)))

(defun org-markdown-preview-dispatch-scroll (value)
  "Scroll to the VALUE."
  (org-markdown-preview--websocket-send-msg-to-client "scroll" value))

(defun org-markdown-preview--scroll ()
  "Scroll preview page to the Emacs position in current buffer."
  (let ((value (org-markdown-preview-calc-size-percent)))
    (org-markdown-preview-dispatch-scroll
     value)))

(defvar org-markdown-preview-scroll-timer nil)
(defvar org-markdown-preview-update-timer nil)

(defun org-markdown-preview-run-in-buffer (buffer fn &rest args)
  "Apply FN with ARGS in BUFFER if it is live."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (apply fn args))))

(defun org-markdown-preview-debounce (timer-sym delay fn &rest args)
  "Debounce execution FN with ARGS for DELAY.
TIMER-SYM is a symbol to use as a timer."
  (when-let* ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value))
    (set timer-sym nil))
  (set timer-sym (apply #'run-with-timer delay
                        nil
                        #'org-markdown-preview-run-in-buffer
                        (current-buffer)
                        fn
                        args)))

(defun org-markdown-preview-refresh ()
  "Refresh preview page.

If value of `org-markdown-preview-refresh-delay' is nil, refresh immediately,
othervise with delay."
  (if org-markdown-preview-refresh-delay
      (org-markdown-preview-debounce
       'org-markdown-preview-update-timer
       org-markdown-preview-refresh-delay
       #'org-markdown-preview-refresh-buffer)
    (org-markdown-preview-refresh-buffer)))

(defun org-markdown-preview-scroll ()
  "Update scroll position in preview page after delay.

Value for delay is stored in `org-markdown-preview-scroll-delay'.

If it is nil, don's update position at all."
  (when org-markdown-preview-scroll-delay
    (org-markdown-preview-debounce 'org-markdown-preview-scroll-timer
                                   org-markdown-preview-scroll-delay
                                   #'org-markdown-preview--scroll)))

;;;###autoload
(defun org-markdown-preview-copy-markdown-as-org ()
  "Copy the selected Markdown region as `org-mode' content."
  (interactive)
  (pcase-let* ((`(,beg . ,end)
                (if (region-active-p)
                    (cons (region-beginning)
                          (region-end))
                  (cons (point-min)
                        (point-max))))
               (content (org-markdown-preview-pandoc-from-string
                         (buffer-substring-no-properties beg end)
                         org-markdown-preview-pandoc-output-type
                         "org")))
    (kill-new content)
    (message "Copied as org")
    content))

;;;###autoload
(defun org-markdown-preview-copy-org-as-markdown ()
  "Copy the selected `org-mode' region as Markdown content."
  (interactive)
  (pcase-let* ((`(,beg . ,end)
                (if (region-active-p)
                    (cons (region-beginning)
                          (region-end))
                  (cons (point-min)
                        (point-max))))
               (content (org-markdown-preview-pandoc-from-string
                         (buffer-substring-no-properties beg end)
                         "org"
                         org-markdown-preview-pandoc-output-type)))
    (kill-new content)
    (message "Copied as markdown")
    content))

;;;###autoload
(defun org-markdown-preview-markdown-write ()
  "Write markdown content to a file if conditions are met."
  (interactive)
  (when (and org-markdown-preview-md-content
             (buffer-live-p org-markdown-preview-preview-buffer)
             (buffer-local-value 'buffer-file-name
                                 org-markdown-preview-preview-buffer)
             (eq 'org-mode
                 (buffer-local-value
                  'major-mode
                  org-markdown-preview-preview-buffer)))
    (write-region org-markdown-preview-md-content nil
                  (concat (file-name-sans-extension
                           (buffer-local-value
                            'buffer-file-name
                            org-markdown-preview-preview-buffer))
                          ".md")
                  nil)))

(defvar org-markdown-preview-html-source-file
  (expand-file-name "markdown-preview.html" org-markdown-preview-data-root)
  "The source file name for `httpd/org-markdown-preview' servlet.")

(defvar org-markdown-preview-html-source-file-gh
  (expand-file-name "markdown-preview-gh.html"
                    org-markdown-preview-data-root)
  "The source file name for `httpd/org-markdown-preview-ghub' servlet.")

(defun org-markdown-preview-servervlet-content (html-file)
  "Return HTML-FILE content for serverlet."
  (let* ((default-port (format "%s" (eval (car (get
                                                'org-markdown-preview-websocket-port
                                                'standard-value)))))
         (port (format "%s" org-markdown-preview-websocket-port))
         (regex (regexp-opt (list default-port) 'symbols)))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (while (re-search-forward regex nil t 1)
        (replace-match port))
      (buffer-string))))

(defun org-markdown-preview-servervlet-pandoc-content ()
  "Return the HTML content for `httpd/org-markdown-preview' servlet."
  (org-markdown-preview-servervlet-content org-markdown-preview-html-source-file))

(defun org-markdown-preview-markdown-it-servervlet-content ()
  "Return the HTML content for `httpd/org-markdown-preview-ghub' serverlet."
  (org-markdown-preview-servervlet-content
   org-markdown-preview-html-source-file-gh))

(defservlet org-markdown-preview
  "text/html; charset=UTF-8" ()
  (insert (org-markdown-preview-servervlet-pandoc-content)))

(defservlet org-markdown-preview-ghub
  "text/html; charset=UTF-8" ()
  (insert (org-markdown-preview-markdown-it-servervlet-content)))

(defun org-markdown-preview-websockets-on-message (_websockets frame)
  "Handle websocket messages on the websocket FRAME struct."
  (condition-case err
      (let ((msg (websocket-frame-payload frame)))
        (pcase msg
          ("getHtml"
           (org-markdown-preview-refresh-buffer))))
    (error (message "%s" err))))



(defun org-markdown-preview--get-status-error (status)
  "Display error details from GitHub code search status.

Argument STATUS is a plist containing the status information, including any
error details."
  (when-let* ((err (plist-get status :error)))
    (concat (propertize
             "org-markdown-preview error: "
             'face
             'error)
            (mapconcat (apply-partially #'format "%s")
                       (delq nil
                             (list (or
                                    (when-let* ((type
                                                 (ignore-errors
                                                   (cadr
                                                    err))))
                                      type)
                                    err)
                                   (ignore-errors (caddr
                                                   err))
                                   (ignore-errors
                                     (alist-get 'message
                                                (car-safe
                                                 (last
                                                  err))))
                                   (ignore-errors
                                     (alist-get 'documentation_url
                                                (car-safe
                                                 (last
                                                  err))))))
                       " "))))

(defun org-markdown-preview--decode-payload (&optional _status)
  "Decode and return UTF-8 string from current buffer position to end."
  (and (not (eobp))
       (decode-coding-string
        (buffer-substring-no-properties (point)
                                        (point-max))
        'utf-8)))


(defun org-markdown-preview--ghub-md-to-html (text callback)
  "Convert Markdown TEXT to HTML using GitHub API and execute CALLBACK.

Argument TEXT is the markdown text to be converted to HTML.

Argument CALLBACK is a function to be called with the HTML result."
  (require 'ghub)
  (ghub-post "/markdown" nil
             :payload `((mode . "gfm")
                        (text . ,text))
             :auth 'none
             :reader #'org-markdown-preview--decode-payload
             :headers `(("Accept" . "application/vnd.github+json"))
             :callback
             (lambda (value _headers status &rest _)
               (if-let ((err
                         (org-markdown-preview--get-status-error
                          status)))
                   (message err)
                 (funcall callback value)))))

(defun org-markdown-preview--on-open (ws)
  "Handle WebSocket connection, send HTML, and scroll preview buffer.

Argument WS is the WebSocket connection that has been opened."
  (message "org-markdown-preview: opened")
  (setq org-markdown-preview-websockets
        (push ws org-markdown-preview-websockets))
  (if org-markdown-preview-use-github-api
      (org-markdown-preview-websocket-send-ghub-html)
    (org-markdown-preview-websocket-send-html))
  (when (buffer-live-p org-markdown-preview-preview-buffer)
    (with-current-buffer org-markdown-preview-preview-buffer
      (org-markdown-preview--scroll))))

(defun org-markdown-preview-run-socket ()
  "Run websocket server on port `org-markdown-preview-websocket-port'."
  (when org-markdown-preview-websocket-server
    (websocket-server-close org-markdown-preview-websocket-server))
  (setq org-markdown-preview-websocket-server
        (websocket-server
         org-markdown-preview-websocket-port
         :host 'local
         :on-message 'org-markdown-preview-websockets-on-message
         :on-open #'org-markdown-preview--on-open
         :on-close (lambda (ws)
                     (message "org-markdown-preview: closed")
                     (setq org-markdown-preview-websockets
                           (delete ws org-markdown-preview-websockets))))))


(defun org-markdown-preview-init ()
  "Initialize markdown preview mode."
  (setq org-markdown-preview-preview-buffer (current-buffer))
  (setf httpd-root default-directory)
  (org-markdown-preview-setup-on)
  (httpd-start)
  (unless org-markdown-preview-websocket-server
    (org-markdown-preview-run-socket))
  (org-markdown-preview-browse-preview))

(defun org-markdown-preview-setup-on ()
  "Add hooks for refreshing and maybe for updating position.

Hook for refreshing content is specified in
`org-markdown-preview-refresh-behavior'.

Hooks for updating position will not be added
if value `org-markdown-preview-scroll-delay' is nil."
  (add-hook org-markdown-preview-refresh-behavior
            #'org-markdown-preview-refresh nil t)
  (when org-markdown-preview-scroll-delay
    (add-hook 'post-command-hook
              #'org-markdown-preview-scroll nil t)))

(defun org-markdown-preview-cleanup-timers ()
  "Cancel refresh and scroll timers."
  (dolist (sym '(org-markdown-preview-update-timer
                 org-markdown-preview-scroll-timer))
    (when (timerp (symbol-value sym))
      (cancel-timer (symbol-value sym))
      (set sym nil))))

(defun org-markdown-preview-setup-off ()
  "Cleanup hooks added by `org-markdown-preview-setup-on'."
  (let ((refresh-syms
         (remove nil
                 (mapcar
                  (lambda (it)
                    (cadr (memq :value it)))
                  (cdr (get 'org-markdown-preview-refresh-behavior
                            'custom-type))))))
    (dolist (sym (append refresh-syms
                         (list org-markdown-preview-refresh-behavior)))
      (remove-hook sym
                   #'org-markdown-preview-refresh-buffer
                   'local))
    (remove-hook 'post-command-hook #'org-markdown-preview-scroll t)))

(defvar org-markdown-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-e")
                #'org-markdown-preview-markdown-write)
    map))

;;;###autoload
(define-minor-mode org-markdown-preview-mode
  "Preview `Org-mode' content as `GitHub-Flavored' Markdown in a web browser.

Enable `org-markdown-preview-mode' to preview Org files in a web browser as
GitHub Flavored Markdown (GFM). This mode uses a local HTTP server and
WebSockets to provide live updates to the preview as the Org file is edited.

When the mode is enabled, it starts an HTTP server, opens the default web
browser to display the preview, and sets up hooks to update the preview and
scroll position in response to changes in the Org file. The preview is generated
using Pandoc, with options customizable via
`org-markdown-preview-pandoc-options'.

The mode also provides several customization options, including
`org-markdown-preview-pandoc-output-type' to specify the output type for Pandoc,
`org-markdown-preview-scroll-delay' to set the delay before updating the scroll
position, `org-markdown-preview-pandoc-options' to specify extra options for
Pandoc, `org-markdown-preview-refresh-behavior' to control when to refresh the
preview page, `org-markdown-preview-refresh-delay' to set the delay before
refreshing the content, `org-markdown-preview-browse-fn' to specify the function
for browsing the preview page, and `org-markdown-preview-websocket-port' to set
the WebSocket port.

When the mode is disabled, it stops the HTTP server, closes the WebSocket
connections, and removes the hooks it has set up."
  :keymap org-markdown-preview-mode-map
  :global nil
  (when (and (buffer-live-p org-markdown-preview-preview-buffer)
             (not (eq (current-buffer) org-markdown-preview-preview-buffer))
             (buffer-local-value 'org-markdown-preview-mode
                                 org-markdown-preview-preview-buffer))
    (with-current-buffer org-markdown-preview-preview-buffer
      (org-markdown-preview-mode -1))
    (setq org-markdown-preview-preview-buffer nil))
  (org-markdown-preview-setup-off)
  (org-markdown-preview-cleanup-timers)
  (setq org-markdown-preview-preview-buffer (current-buffer))
  (when org-markdown-preview-websocket-server
    (websocket-server-close org-markdown-preview-websocket-server)
    (setq org-markdown-preview-websocket-server nil))
  (httpd-stop)
  (when org-markdown-preview-mode
    (org-markdown-preview-init)))

(provide 'org-markdown-preview)
;;; org-markdown-preview.el ends here