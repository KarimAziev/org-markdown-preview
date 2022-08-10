;;; org-markdown-preview.el --- A minor mode for preview markdown in org mode

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-markdown-preview
;; Version: 0.1.0
;; Keywords: outlines convenience docs
;; Package-Requires: ((emacs "27.1"))

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

;; M-x `org-markdown-preview-current-buffer'
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
(require 'org)
(require 'markdown-mode)

(defvar org-markdown-preview-websockets nil)
(defvar org-markdown-preview-websocket-server nil)
(defvar org-markdown-preview-markdown-current-html nil)

(defvar org-markdown-preview-data-root (file-name-directory (if load-in-progress
                                                                load-file-name
                                                              (buffer-file-name))))

(defcustom org-markdown-preview-edit-persistent-message t
  "Whether to show persistent exit help while editing markdown in org buffer.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'org-markdown-preview
  :type 'boolean)

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

(defcustom org-markdown-preview-browse-url-function 'browse-url
  "Function for opening preview."
  :type 'function
  :group 'org-markdown-preview)

(defvar org-markdown-after-websockets-send-message-hook nil
  "Hooks run after sending message to websocket client.")

(defvar org-markdown-preview-source-buffer nil)
(defvar org-markdown-preview-org-buffer nil)
(defvar org-markdown-preview-md-buffer nil)
(defvar org-markdown-preview-md-content nil)

;;;###autoload
(defun org-markdown-preview-browse-preview ()
	"Visit a served page in a browser.
Uses `browse-url' to launch a browser"
  (interactive)
  (let* ((proc (get-process "httpd"))
         (proc-info (process-contact proc t))
         (raw-host (plist-get proc-info :host))
         (host (if (member raw-host
                           '(nil local "127.0.0.1" "::1" "0.0.0.0" "::"))
                   "localhost"
                 raw-host))
         (local-addr (plist-get proc-info :local))
         (port (aref local-addr (1- (length local-addr))))
         (url (format "http://%s:%d/org-markdown-preview/preview"
                      host port)))
    (funcall org-markdown-preview-browse-url-function url)))

(defvar org-markdown-preview-edit-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'org-markdown-preview-markdown-write)
    (define-key map (kbd "C-c C-k") #'kill-this-buffer)
    (define-key map (kbd "C-x 0") #'kill-this-buffer)
    map))

(defun org-markdown-preview-websocket-send-msg-to-client (type &optional
                                                               payload)
  "Notify all opened sockets with message TYPE and PAYLOAD."
  (setq org-markdown-preview-websockets
        (seq-filter #'websocket-openp org-markdown-preview-websockets))
  (dolist (socket org-markdown-preview-websockets)
    (when (and socket type)
      (message "sending %s type" type)
      (websocket-send-text
       socket
       (json-encode
        `(("type" . ,type)
          ("payload" . ,payload))))
      (run-hooks 'org-markdown-after-websockets-send-message-hook))))

(defun org-markdown-preview-pandoc-from-string (string input-type output-type
                                                       &rest options)
  "Execute `pandoc' on STRING in INPUT-TYPE to OUTPUT-TYPE additional OPTIONS."
  (setq options (delete nil (flatten-list options)))
  (let ((args (append
               (list "pandoc" t t nil)
               (list "-f" input-type "-t"
                     output-type)
               options)))
    (with-temp-buffer
      (insert string)
      (if (eq 0
              (apply #'call-process-region (append (list (point-min)
                                                         (point-max))
                                                   args)))
          (buffer-string)
        (error (buffer-string))))))

(defun org-markdown-preview-send-html ()
  (when-let ((html (or org-markdown-preview-markdown-current-html
                       (org-markdown-preview-get-html))))
    (org-markdown-preview-websocket-send-msg-to-client
     "refresh"
     html)))

;;;###autoload
(defun org-markdown-preview-markdown-write ()
  "Write `org-markdown-preview-md-content' to the markdown file.
Th file name based on the local value of `org-markdown-preview-org-buffer'."
  (interactive)
  (kill-buffer)
  (write-region org-markdown-preview-md-content nil
                (concat (file-name-sans-extension
                         (or (buffer-local-value
                              'buffer-file-name
                              org-markdown-preview-org-buffer)
                             (expand-file-name
                              (replace-regexp-in-string
                               "\\*" ""
                               (buffer-name
                                org-markdown-preview-org-buffer))
                              default-directory)))
                        ".md")
                nil))

;;;###autoload
(defun org-markdown-preview-current-buffer ()
	"Write org content to markdown file and refresh websocket clients.
Org content is taken from `org-markdown-preview-org-buffer'."
  (interactive)
  (org-markdown-preview-setup-buffers)
  (with-current-buffer org-markdown-preview-source-buffer
    (setq org-markdown-preview-md-content nil)
    (setq org-markdown-preview-markdown-current-html
          (org-markdown-preview-get-html)))
  (when org-markdown-preview-md-content
    (pcase (buffer-local-value 'major-mode
                               org-markdown-preview-source-buffer)
      ((or 'org-mode 'outline-mode)
       (pop-to-buffer-same-window
        (with-current-buffer (get-buffer-create
                              "*org-markdown-preview-md*")
          (erase-buffer)
          (goto-char (point-min))
          (insert org-markdown-preview-md-content)
          (set-buffer-modified-p nil)
          (delay-mode-hooks
            (pcase org-markdown-preview-pandoc-output-type
              ((or "gfm" "markdown_mmd")
               (gfm-mode))
              (_ (markdown-mode))))
          (use-local-map
           (let ((map (copy-keymap org-markdown-preview-edit-buffer-map)))
             (set-keymap-parent map (current-local-map))
             map))
          (when org-markdown-preview-edit-persistent-message
            (setq header-line-format
	                (substitute-command-keys
	                 "Save with `\\[org-markdown-preview-markdown-write]' or abort with \
`\\[kill-this-buffer]'")))
          (current-buffer))))))
  (org-markdown-preview-run)
  (if org-markdown-preview-markdown-current-html
      (org-markdown-preview-websocket-send-msg-to-client
       "refresh"
       org-markdown-preview-markdown-current-html)
    (message "No html")))

(defun org-markdown-preview-org-to-md (org-content)
  "Transform ORG-CONTENT with pandoc to markdown.
See `org-markdown-preview-pandoc-output-type'."
  (org-markdown-preview-pandoc-from-string
   org-content
   "org"
   org-markdown-preview-pandoc-output-type))

(defun org-markdown-preview-md-to-org (md-content)
  "Transform MD-CONTENT with pandoc to markdown.
See `org-markdown-preview-pandoc-output-type'."
  (org-markdown-preview-pandoc-from-string
   md-content
   org-markdown-preview-pandoc-output-type
   "org"))

(defun org-markdown-preview-md-to-html (md-content)
  "Transform MD-CONTENT with pandoc to markdown.
See `org-markdown-preview-pandoc-output-type'."
  (org-markdown-preview-pandoc-from-string
   md-content
   org-markdown-preview-pandoc-output-type
   "html5"))

(defun org-markdown-preview-org-to-html (org-content)
  "Transform ORG-CONTENT with pandoc to html5."
  (setq org-markdown-preview-md-content
        (org-markdown-preview-org-to-md org-content))
  (org-markdown-preview-md-to-html
   org-markdown-preview-md-content))

(require 'dom)

(defun org-markdown-preview-get-html ()
  "Convert buffer contents to html with pandoc."
  (when-let ((fn (pcase major-mode
                   ('org-mode 'org-markdown-preview-org-to-html)
                   ((or 'markdown-mode 'gfm-mode)
                    'org-markdown-preview-md-to-html)
                   ((or 'html 'web-mode)
                    (lambda (html)
                      (or (when-let* ((dom (with-temp-buffer (insert html)
                                                             (libxml-parse-html-region
                                                              (point-min) (point-max))))
                                      (body (dom-by-tag
                                             dom 'body))
                                      (children (dom-children body)))
                            (with-temp-buffer
                              (dolist (child children)
                                (when (listp child)
                                  (dom-print child)))
                              (buffer-string)))
                          html))))))
    (funcall fn (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun org-markdown-preview-browse-md-content (markdown-content)
	"Preview MARKDOWN-CONTENT with html."
  (interactive)
  (when-let ((html-result (org-markdown-preview-pandoc-from-string
                           markdown-content
                           org-markdown-preview-pandoc-output-type
                           "html5")))
    (unless org-markdown-preview-websocket-server
      (org-markdown-preview-run-socket))
    (unless (process-status "httpd")
      (httpd-start))
    (org-markdown-preview-browse-preview)
    (org-markdown-preview-websocket-send-msg-to-client
     "refresh"
     html-result)))

(defvar org-markdown-preview-html-source-file
  (expand-file-name "markdown-preview.html" org-markdown-preview-data-root)
  "Source file name or buffer for `httpd/skewer/demo' servlet.")

(defservlet org-markdown-preview/preview
  "text/html; charset=UTF-8" ()
  (insert-file-contents org-markdown-preview-html-source-file))

(defun org-markdown-preview-websockets-on-message (_websockets frame)
	"Handle websocket messages on the websocket FRAME struct."
  (condition-case err
      (let ((msg (websocket-frame-payload frame)))
        (pcase msg
          ("getHtml" (org-markdown-preview-current-buffer))))
    (error (message "%s" err))))

(defun org-markdown-preview-cleanup ()
  "Close websocket server and reset `org-markdown-preview-org-buffer'."
  (when org-markdown-preview-websocket-server
    (unless (eq 'closed (process-status org-markdown-preview-websocket-server))
      (websocket-server-close org-markdown-preview-websocket-server)))
  (setq org-markdown-preview-websocket-server nil)
  (setq org-markdown-preview-org-buffer nil))

;;;###autoload
(defun org-markdown-preview-edit-md-region ()
  "Edit current buffer with markdown in org buffer."
  (interactive)
  (let ((buff (current-buffer))
        (pos (point))
        (md-content (buffer-substring-no-properties (point-min) (point-max)))
        (org-content))
    (setq org-markdown-preview-source-buffer buff)
    (setq org-content
          (if (string-empty-p (string-trim md-content))
              ""
            (or (org-markdown-preview-pandoc-from-string
                 md-content org-markdown-preview-pandoc-output-type "org")
                "")))
    (when-let ((wind (get-buffer-window buff)))
      (unless (eq wind (selected-window))
        (select-window wind))
      (pop-to-buffer-same-window
       (with-current-buffer (get-buffer-create "*org-markdown-preview*")
         (erase-buffer)
         (insert org-content)
         (unless (> pos (point-max))
           (goto-char pos))
         (delay-mode-hooks
           (org-mode))
         (org-markdown-preview-edit-mode)
         org-markdown-preview-org-buffer)
       t))))

(define-minor-mode org-markdown-preview-edit-mode
  "Minor mode for editing markdown in org buffers.

\\{org-markdown-preview-edit-buffer-map}."
  :keymap org-markdown-preview-edit-buffer-map
  :lighter " OrgMarkdown"
  (setq org-markdown-preview-source-buffer (current-buffer))
  (add-hook 'kill-buffer-hook 'org-markdown-preview-cleanup
            nil t)
  (when org-markdown-preview-edit-persistent-message
    (setq header-line-format
	        (substitute-command-keys
	         "Edit, then exit with `\\[org-markdown-preview-markdown-write]' or abort with \
`\\[kill-this-buffer]'"))))

(defun org-markdown-preview-run-socket ()
	"Run websocket server on port 8080."
  (setq org-markdown-preview-websocket-server
        (websocket-server
         8080
         :host 'local
         :on-message 'org-markdown-preview-websockets-on-message
         :on-open (lambda (ws)
                    (message "websocket opened  %s" ws)
                    (setq org-markdown-preview-websockets
                          (push ws org-markdown-preview-websockets))
                    (when org-markdown-preview-markdown-current-html
                      (org-markdown-preview-websocket-send-msg-to-client
                       "refresh"
                       org-markdown-preview-markdown-current-html)))
         :on-close (lambda (ws)
                     (message "websocket closed  %s" ws)
                     (setq org-markdown-preview-websockets
                           (delete ws org-markdown-preview-websockets))))))

(defun org-markdown-preview-setup-buffers ()
  (let ((buff (current-buffer)))
    (setq org-markdown-preview-source-buffer buff)
    (pcase major-mode
      ((or 'org-mode 'outline-mode)
       (unless (eq org-markdown-preview-org-buffer buff)
         (when (bufferp org-markdown-preview-org-buffer)
           (kill-buffer org-markdown-preview-org-buffer))
         (when (bufferp org-markdown-preview-md-buffer)
           (kill-buffer org-markdown-preview-md-buffer))
         (setq org-markdown-preview-org-buffer buff)
         (setq org-markdown-preview-md-buffer nil)))
      ((or 'markdown-mode
           'gfm-mode
           'gfm-view-mode)
       (unless (eq org-markdown-preview-md-buffer buff)
         (when (bufferp org-markdown-preview-md-buffer)
           (kill-buffer org-markdown-preview-md-buffer))
         (when (bufferp org-markdown-preview-org-buffer)
           (kill-buffer org-markdown-preview-org-buffer))
         (setq org-markdown-preview-org-buffer nil)
         (setq org-markdown-preview-md-buffer buff))))))

(defun org-markdown-preview-run ()
  "Run httpd server and websockets."
  (interactive)
  (when (or (not org-markdown-preview-websocket-server)
            (eq (process-status org-markdown-preview-websocket-server)
                'listen))
    (when org-markdown-preview-websocket-server
      (websocket-server-close
       org-markdown-preview-websocket-server)
      (setq org-markdown-preview-websocket-server nil))
    (org-markdown-preview-run-socket))
  (unless (eq (process-status "httpd")
              'listen)
    (httpd-stop)
    (httpd-start))
  (setq org-markdown-preview-websockets
        (seq-filter #'websocket-openp org-markdown-preview-websockets))
  (unless org-markdown-preview-websockets
    (org-markdown-preview-browse-preview)))

(defun org-markdown-preview-init ()
	"Initialize markdown preview mode."
  (remove-hook 'after-save-hook
               'org-markdown-preview-current-buffer
               'local)
  (add-hook 'after-save-hook
            'org-markdown-preview-current-buffer
            nil 'local)
  (org-markdown-preview-setup-buffers))

;;;###autoload
(define-minor-mode org-markdown-preview-mode
  "Export org file to markdown on save, starts server with hot-reloading.
Uses `browse-url' to launch a browser."
  :global nil
  (if org-markdown-preview-mode
      (org-markdown-preview-init)
    (remove-hook 'after-save-hook
                 'org-markdown-preview-current-buffer
                 'local)
    (when org-markdown-preview-websocket-server
      (websocket-server-close org-markdown-preview-websocket-server)
      (setq org-markdown-preview-websocket-server nil))
    (httpd-stop)))

(provide 'org-markdown-preview)
;;; org-markdown-preview.el ends here