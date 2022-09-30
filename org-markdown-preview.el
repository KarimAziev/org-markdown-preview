;;; org-markdown-preview.el --- A minor mode for preview markdown in org mode -*- lexical-binding: t; -*-

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
         (port (aref local-addr (1- (length local-addr)))))
    (format "http://%s:%d/org-markdown-preview/preview"
            host port)))

;;;###autoload
(defun org-markdown-preview-browse-preview ()
  "Visit a served page in a browser.
Uses `browse-url' to launch a browser"
  (interactive)
  (let* ((url (org-markdown-preview-get-url))
         (orig-wind (selected-window))
         (wind-target (if (minibuffer-window-active-p orig-wind)
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
                           (split-window-right))))))
    (with-selected-window wind-target
      (browse-url url))))

(defun org-markdown-preview-websocket-send-msg-to-client (type &optional
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
      (cons
       (eq 0
           (apply #'call-process-region (append (list (point-min)
                                                     (point-max))
                                               args)))
       (buffer-string)))))

;;;###autoload
(defun org-markdown-preview-websocket-send-html ()
	"Write org content to markdown file and refresh websocket clients.
Org content is taken from `org-markdown-preview-preview-buffer'."
  (interactive)
  (let* ((content (with-current-buffer org-markdown-preview-preview-buffer
                    (buffer-substring-no-properties (point-min) (point-max))))
         (md-result (org-markdown-preview-pandoc-from-string
                     content "org" org-markdown-preview-pandoc-output-type))
         (html-result (when (car md-result)
                        (org-markdown-preview-pandoc-from-string
                         (cdr md-result)
                         org-markdown-preview-pandoc-output-type
                         "html5"))))
    (setq org-markdown-preview-md-content (when (car md-result)
                                            (cdr md-result)))
    (setq org-markdown-preview-markdown-current-html
          (when (and html-result
                     (car html-result))
            (cdr html-result)))
    (if (null org-markdown-preview-markdown-current-html)
        (message "Failed 1) %s 2) %s" (cdr md-result)
                 (when html-result
                   (cdr html-result)))
      (org-markdown-preview-websocket-send-msg-to-client
       "refresh"
       org-markdown-preview-markdown-current-html)
      (org-markdown-preview-markdown-write))))

;;;###autoload
(defun org-markdown-preview-markdown-write ()
  "Write `org-markdown-preview-md-content' to the markdown file.
Th file name based on the local value of `org-markdown-preview-preview-buffer'."
  (interactive)
  (when (and org-markdown-preview-md-content
             org-markdown-preview-preview-buffer
             (buffer-local-value 'buffer-file-name
                                 org-markdown-preview-preview-buffer))
    (write-region org-markdown-preview-md-content nil
                  (concat (file-name-sans-extension
                           (buffer-local-value
                            'buffer-file-name
                            org-markdown-preview-preview-buffer))
                          ".md")
                  nil)))

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
          ("getHtml" (org-markdown-preview-websocket-send-html))))
    (error (message "%s" err))))

(defun org-markdown-preview-run-socket ()
	"Run websocket server on port 8080."
  (setq org-markdown-preview-websocket-server
        (websocket-server
         8080
         :host 'local
         :on-message 'org-markdown-preview-websockets-on-message
         :on-open (lambda (ws)
                    (message "websocket opened")
                    (setq org-markdown-preview-websockets
                          (push ws org-markdown-preview-websockets)))
         :on-close (lambda (ws)
                     (message "websocket closed")
                     (setq org-markdown-preview-websockets
                           (delete ws org-markdown-preview-websockets))))))

(defun org-markdown-preview-init ()
	"Initialize markdown preview mode."
  (remove-hook 'after-save-hook
               'org-markdown-preview-websocket-send-html
               'local)
  (add-hook 'after-save-hook
            'org-markdown-preview-websocket-send-html
            nil 'local)
  (setq org-markdown-preview-preview-buffer (current-buffer))
  (unless org-markdown-preview-websocket-server
    (org-markdown-preview-run-socket))
  (unless (process-status "httpd")
    (httpd-start))
  (org-markdown-preview-browse-preview))

;;;###autoload
(define-minor-mode org-markdown-preview-mode
  "Export org file to markdown on save, starts server with hot-reloading.
Uses `browse-url' to launch a browser."
  :global nil
  (if org-markdown-preview-mode
      (org-markdown-preview-init)
    (remove-hook 'after-save-hook
                 'org-markdown-preview-websocket-send-html
                 'local)
    (when org-markdown-preview-websocket-server
      (websocket-server-close org-markdown-preview-websocket-server)
      (setq org-markdown-preview-websocket-server nil))
    (httpd-stop)))

(provide 'org-markdown-preview)
;;; org-markdown-preview.el ends here