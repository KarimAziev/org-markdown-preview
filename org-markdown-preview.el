;;; org-markdown-preview.el --- Preview Markdown from Org or Markdown buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-markdown-preview
;; Version: 0.1.0
;; Keywords: outlines convenience docs
;; Package-Requires: ((emacs "28.1") (websocket "1.15") (simple-httpd "1.5.1"))
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

;; Live preview of Org and Markdown buffers in a web browser.

;;; Commands

;; M-x `org-markdown-preview-mode'
;;      Enable live preview for the current Org or Markdown buffer.

;; M-x `org-markdown-preview-browse-preview'
;;      Open the preview page in a browser again.
;;
;; M-x `org-markdown-preview-markdown-write'
;;      Write `org-markdown-preview-md-content' to a sibling Markdown file.
;;
;; M-x `org-markdown-preview-copy-org-as-markdown'
;;      Copy the active Org region as Markdown.
;;
;; M-x `org-markdown-preview-copy-markdown-as-org'
;;      Copy the active Markdown region as Org.
;;
;; M-x `org-markdown-preview-copy-html-as-org'
;;      Copy the active HTML region as Org.
;;
;; M-x `org-markdown-preview-copy-html-as-markdown'
;;      Copy the active HTML region as Markdown.

;;; Customization

;; `org-markdown-preview-pandoc-output-type'
;;      Markdown output format passed to `pandoc'

;;; Code:

(require 'websocket)
(require 'simple-httpd)

(declare-function ghub-post "ghub")

(defvar org-markdown-preview-websockets nil
  "List of active preview WebSocket clients.")

(defvar org-markdown-preview-websocket-server nil
  "WebSocket server instance used by the preview.")

(defvar org-markdown-preview-markdown-current-html nil
  "Most recently rendered HTML sent to preview clients.")

(defconst org-markdown-preview-data-root
  (file-name-directory (if (bound-and-true-p load-file-name) load-file-name
                         (buffer-file-name)))
  "Directory containing the `org-markdown-preview' package files.")

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
  "Whether to use the GitHub API to render preview HTML.

Requires `ghub' to be installed."
  :type 'boolean
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-scroll-delay 0.5
  "Number of seconds to wait after the last command before syncing scroll.
If nil, disable scroll synchronization entirely."
  :group 'org-markdown-preview
  :type '(radio  (const
                  :tag "None (disable scroll)"
                  :value nil)
                 (number
                  :tag "Seconds")))

(defcustom org-markdown-preview-pandoc-options '("--quiet"
                                                 "--embed-resources"
                                                 "--standalone"
                                                 "--syntax-highlighting=zenburn"
                                                 "--wrap=none")
  "Additional command-line options passed to Pandoc."
  :group 'org-markdown-preview
  :type '(repeat string))

(defcustom org-markdown-preview-refresh-behavior 'after-save-hook
  "Hook on which to refresh the preview page.
The value must be a hook variable symbol to which the refresh handler is added."
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
  "Number of seconds to wait before refreshing preview content.
If nil, refresh immediately after `org-markdown-preview-refresh-behavior' runs."
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
  "Function used to open the preview page.

The function is called with one argument, the URL to open.

By default, use xwidgets when available and `browse-url' otherwise."
  :type '(radio  (function-item org-markdown-preview-browse-with-xwidget)
                 (function-item browse-url)
                 (function
                  :tag "Custom function"))
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-websocket-port 7071
  "Port used by the preview WebSocket server."
  :type 'integer
  :group 'org-markdown-preview)

(defcustom org-markdown-preview-preprocess-org-content-hook '(org-markdown-preview-preprocess-org-content)
  "Hook run before Org content is converted for preview.

Each function in the hook should accept no arguments and will be called in the
temporary buffer before Pandoc processes it."
  :group 'org-markdown-preview
  :type 'hook)

(defcustom org-markdown-preview-post-process-org-content-hook '(org-markdown-preview-strip-propererties)
  "Hook run after converting content to Org in a temporary buffer.

Each function in the hook should accept no arguments and will be called in the
temporary buffer before the converted content is returned."
  :group 'org-markdown-preview
  :type 'hook)


(defcustom org-markdown-preview-post-process-md-content-hook '(org-markdown-preview-cleanup-escape-sequences)
  "Hook run after converting content to Markdown in a temporary buffer.

Each function in the hook should accept no arguments and will be called in the
temporary buffer before the converted content is returned."
  :group 'org-markdown-preview
  :type 'hook)

(defun org-markdown-preview--get-url ()
  "Return the URL of the preview page served by `simple-httpd'."
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
  "Open URL in an xwidget browser in another window."
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
  "Open the preview page in a browser."
  (interactive)
  (if-let* ((url (org-markdown-preview--get-url)))
      (funcall org-markdown-preview-browse-fn url)
    (user-error
     "org-markdown-preview: couldn't resolve URL; ensure that httpd is running")))

(defun org-markdown-preview--websocket-send-msg-to-client (type &optional
                                                                payload)
  "Send all open preview clients a message with TYPE and PAYLOAD."
  (setq org-markdown-preview-websockets
        (seq-filter #'websocket-openp org-markdown-preview-websockets))
  (dolist (socket org-markdown-preview-websockets)
    (when (and socket type)
      (websocket-send-text
       socket
       (json-encode
        `(("type" . ,type)
          ("payload" . ,payload)))))))

(defvar org-markdown-preview-preview-buffer nil
  "Buffer currently associated with the preview session.")

(defvar org-markdown-preview-md-content nil
  "Most recently generated Markdown content for the preview buffer.")

(defun org-markdown-preview-strip-propererties ()
  "Remove selected Org property drawers from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward
              ":properties:[\n]+[\s\t]*:custom_id:\\([\s\t]\\)*[^\n]+[\n]+[\s\t]*:end:[\n]"
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
  "Replace `emacs-lisp' with `elisp' in Org source block headers.
Pandoc otherwise converts `emacs-lisp' to `commonlisp' in Markdown output."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "#\\+begin_src[\s]+\\_<\\(emacs-lisp\\)\\_>" nil
                                t
                                1)
        (replace-match "elisp" nil nil nil 1)))))

(defun org-markdown-preview-pandoc-from-string (string input-type output-type)
  "Run Pandoc on STRING, converting from INPUT-TYPE to OUTPUT-TYPE."
  (let ((args (append
               (list "pandoc" t t nil)
               (list "-f" input-type "-t"
                     output-type)
               org-markdown-preview-pandoc-options)))
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
  "Convert the current buffer to Markdown and cache the result."
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

(defun org-markdown-preview--add-ids-to-links (html-str)
  "Add unique `id' attributes to heading tags in HTML-STR."
  (let ((pos 0)
        ;; Heading tags may arrive in mixed case depending on the renderer.
        (case-fold-search t)
        (counter (make-hash-table :test 'equal))
        (dash-protection-sym
         "09876543214b825dc642cb6eb9a060e54bf8d69288fbee49041234567890")
        (underscore-protection-symbol
         "afec96cafb7bc4b0e216bfe86db4bd6c4aab44bca19dd9999b11e162f595d711"))
    (while (string-match "<\\(h[1-6]\\)\\([^>]*\\)>\\([^<]+\\)</\\1>" html-str
                         pos)
      (let* ((tag (match-string 1 html-str))
             (attrs (match-string 2 html-str))
             (text (match-string 3 html-str))
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
        ;; Preserve duplicate headings by suffixing later IDs.
        (while (gethash id counter)
          (setq dup-count (1+ dup-count))
          (setq id (format "%s-%d" base-id dup-count)))
        (puthash id t counter)
        (setq html-str
              (replace-match
               (format "<%s id=\"%s\"%s>%s</%s>" tag id attrs text tag)
               t t html-str))
        (setq pos (match-end 0))))
    html-str))


(defun org-markdown-preview-websocket-send-html ()
  "Render the preview buffer locally and push the resulting HTML to clients."
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
               (org-markdown-preview--add-ids-to-links value))
         (org-markdown-preview--websocket-send-msg-to-client
          "html"
          org-markdown-preview-markdown-current-html))))))

(defun org-markdown-preview-refresh-buffer ()
  "Refresh the browser preview using the configured rendering backend."
  (if org-markdown-preview-use-github-api
      (org-markdown-preview-websocket-send-ghub-html)
    (org-markdown-preview-websocket-send-html)))

(defun org-markdown-preview-calc-size-percent ()
  "Return the current window position as a scroll ratio string.
The value is formatted as a number between 0 and 1."
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

(defun org-markdown-preview--dispatch-scroll (value)
  "Send scroll VALUE to connected preview clients."
  (org-markdown-preview--websocket-send-msg-to-client "scroll" value))

(defun org-markdown-preview--scroll ()
  "Scroll the preview page to match point in the current buffer."
  (let ((value (org-markdown-preview-calc-size-percent)))
    (org-markdown-preview--dispatch-scroll
     value)))

(defvar org-markdown-preview--scroll-timer nil
  "Debounce timer used for scroll synchronization.")

(defvar org-markdown-preview--update-timer nil
  "Debounce timer used for preview refresh.")

(defun org-markdown-preview--run-in-buffer (buffer fn &rest args)
  "Apply FN with ARGS in BUFFER if it is live."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (apply fn args))))

(defun org-markdown-preview--debounce (timer-sym delay fn &rest args)
  "Debounce FN with ARGS by DELAY seconds using TIMER-SYM."
  (when-let* ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value))
    (set timer-sym nil))
  (set timer-sym (apply #'run-with-timer delay
                        nil
                        #'org-markdown-preview--run-in-buffer
                        (current-buffer)
                        fn
                        args)))

(defun org-markdown-preview-refresh ()
  "Refresh the preview page immediately or after a debounce delay."
  (if org-markdown-preview-refresh-delay
      (org-markdown-preview--debounce
       'org-markdown-preview--update-timer
       org-markdown-preview-refresh-delay
       #'org-markdown-preview-refresh-buffer)
    (org-markdown-preview-refresh-buffer)))

(defun org-markdown-preview-scroll ()
  "Update the preview scroll position after `org-markdown-preview-scroll-delay'."
  (when org-markdown-preview-scroll-delay
    (org-markdown-preview--debounce 'org-markdown-preview--scroll-timer
                                   org-markdown-preview-scroll-delay
                                   #'org-markdown-preview--scroll)))


(defun org-markdown-preview--get-region ()
  "Return region bounds if active, otherwise return whole buffer bounds."
  (if (region-active-p)
      (cons (region-beginning)
            (region-end))
    (cons (point-min)
          (point-max))))

(defun org-markdown-preview--copy-region-as (input-format out-format)
  "Convert the region between formats and copy the result to the clipboard.

Argument INPUT-FORMAT is a pandoc input format string.

Argument OUT-FORMAT is a pandoc output format string."
  (pcase-let* ((`(,beg . ,end)
                (org-markdown-preview--get-region))
               (str (buffer-substring-no-properties beg end))
               (content (if (string-empty-p (string-trim str))
                            (user-error "The selected region is empty")
                          (org-markdown-preview-pandoc-from-string
                           str
                           input-format
                           out-format))))
    (kill-new content)
    (message "Copied `%s' region as `%s'" input-format out-format)
    content))

;;;###autoload
(defun org-markdown-preview-copy-markdown-as-org ()
  "Copy the selected Markdown region as `org-mode' content."
  (interactive)
  (org-markdown-preview--copy-region-as
   org-markdown-preview-pandoc-output-type
   "org"))

;;;###autoload
(defun org-markdown-preview-copy-org-as-markdown ()
  "Copy the selected `org-mode' region as Markdown content."
  (interactive)
  (org-markdown-preview--copy-region-as
   "org"
   org-markdown-preview-pandoc-output-type))

;;;###autoload
(defun org-markdown-preview-copy-html-as-markdown ()
  "Copy the selected `html' region as Markdown content."
  (interactive)
  (org-markdown-preview--copy-region-as
   "html"
   org-markdown-preview-pandoc-output-type))

;;;###autoload
(defun org-markdown-preview-copy-html-as-org ()
  "Copy the selected HTML region as Org content."
  (interactive)
  (org-markdown-preview--copy-region-as
   "html"
   "org"))

;;;###autoload
(defun org-markdown-preview-markdown-write ()
  "Write preview Markdown to a sibling `.md' file when possible."
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
  "Source HTML file used by the `httpd/org-markdown-preview' servlet.")

(defvar org-markdown-preview-html-source-file-gh
  (expand-file-name "markdown-preview-gh.html"
                    org-markdown-preview-data-root)
  "Source HTML file used by the `httpd/org-markdown-preview-ghub' servlet.")

(defun org-markdown-preview--servervlet-content-for (html-file)
  "Return servlet HTML from HTML-FILE with the configured WebSocket port."
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

(defun org-markdown-preview--servervlet-pandoc-content ()
  "Return HTML for the `httpd/org-markdown-preview' servlet."
  (org-markdown-preview--servervlet-content-for org-markdown-preview-html-source-file))

(defun org-markdown-preview--gh-servervlet-content ()
  "Return HTML for the `httpd/org-markdown-preview-ghub' servlet."
  (org-markdown-preview--servervlet-content-for
   org-markdown-preview-html-source-file-gh))

(defservlet org-markdown-preview
  "text/html; charset=UTF-8" ()
  (insert (org-markdown-preview--servervlet-pandoc-content)))

(defservlet org-markdown-preview-ghub
  "text/html; charset=UTF-8" ()
  (insert (org-markdown-preview--gh-servervlet-content)))

(defun org-markdown-preview--websockets-on-message (_websockets frame)
  "Handle preview WebSocket messages from FRAME."
  (condition-case err
      (let ((msg (websocket-frame-payload frame)))
        (pcase msg
          ("getHtml"
           (org-markdown-preview-refresh-buffer))))
    (error (message "%s" err))))



(defun org-markdown-preview--get-status-error (status)
  "Return an error string extracted from GitHub API STATUS.

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
  "Convert Markdown TEXT to HTML with the GitHub API, then run CALLBACK.

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
               (if-let* ((err
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

(defun org-markdown-preview--run-socket ()
  "Start the preview WebSocket server on `org-markdown-preview-websocket-port'."
  (when org-markdown-preview-websocket-server
    (websocket-server-close org-markdown-preview-websocket-server))
  (setq org-markdown-preview-websocket-server
        (websocket-server
         org-markdown-preview-websocket-port
         :host 'local
         :on-message 'org-markdown-preview--websockets-on-message
         :on-open #'org-markdown-preview--on-open
         :on-close (lambda (ws)
                     (message "org-markdown-preview: closed")
                     (setq org-markdown-preview-websockets
                           (delete ws org-markdown-preview-websockets))))))


(defun org-markdown-preview--init ()
  "Initialize live preview for the current buffer."
  (setq org-markdown-preview-preview-buffer (current-buffer))
  (setf httpd-root default-directory)
  (org-markdown-preview--setup-on)
  (httpd-start)
  (unless org-markdown-preview-websocket-server
    (org-markdown-preview--run-socket))
  (org-markdown-preview-browse-preview))

(defun org-markdown-preview--setup-on ()
  "Install buffer-local hooks used by `org-markdown-preview-mode'.

Refresh hooks are controlled by `org-markdown-preview-refresh-behavior'.
Scroll hooks are installed only when
`org-markdown-preview-scroll-delay' is non-nil."
  (add-hook org-markdown-preview-refresh-behavior
            #'org-markdown-preview-refresh nil t)
  (when org-markdown-preview-scroll-delay
    (add-hook 'post-command-hook
              #'org-markdown-preview-scroll nil t)))

(defun org-markdown-preview--cleanup-timers ()
  "Cancel refresh and scroll timers."
  (dolist (sym '(org-markdown-preview--update-timer
                 org-markdown-preview--scroll-timer))
    (when (timerp (symbol-value sym))
      (cancel-timer (symbol-value sym))
      (set sym nil))))

(defun org-markdown-preview--setup-off ()
  "Remove hooks added by `org-markdown-preview--setup-on'."
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
  "Preview the current Org or Markdown buffer in a web browser.

The mode starts a local HTTP server, opens the preview page, and keeps the
rendered HTML in sync over WebSockets. Org buffers are converted to Markdown
with Pandoc before rendering. Markdown buffers are rendered directly.

When `org-markdown-preview-use-github-api' is non-nil, HTML is rendered through
GitHub's Markdown API. Otherwise, the package renders HTML locally with Pandoc.

Disabling the mode stops the server, closes WebSocket connections, and removes
all buffer-local hooks and timers created for the preview session."
  :keymap org-markdown-preview-mode-map
  :global nil
  (when (and (buffer-live-p org-markdown-preview-preview-buffer)
             (not (eq (current-buffer) org-markdown-preview-preview-buffer))
             (buffer-local-value 'org-markdown-preview-mode
                                 org-markdown-preview-preview-buffer))
    (with-current-buffer org-markdown-preview-preview-buffer
      (org-markdown-preview-mode -1))
    (setq org-markdown-preview-preview-buffer nil))
  (org-markdown-preview--setup-off)
  (org-markdown-preview--cleanup-timers)
  (setq org-markdown-preview-preview-buffer (current-buffer))
  (when org-markdown-preview-websocket-server
    (websocket-server-close org-markdown-preview-websocket-server)
    (setq org-markdown-preview-websocket-server nil))
  (httpd-stop)
  (when org-markdown-preview-mode
    (org-markdown-preview--init)))

(provide 'org-markdown-preview)
;;; org-markdown-preview.el ends here
