;;; org-markdown-preview-test.el --- Tests for org-markdown-preview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'org-markdown-preview)

(ert-deftest org-markdown-preview-test-markdown-major-modes ()
  "Recognize and preview both Markdown major-mode implementations."
  (dolist (mode '(markdown-mode markdown-ts-mode))
    (with-temp-buffer
      (let ((major-mode mode)
            (org-markdown-preview-post-process-md-content-hook nil)
            (content "# Heading\n\nBody\n"))
        (insert content)
        (should (org-markdown-preview--markdown-buffer-p))
        (should (org-markdown-preview--supported-buffer-p))
        (org-markdown-preview--refresh-buffer-0)
        (should (equal org-markdown-preview-md-content content))))))

(ert-deftest org-markdown-preview-test-markdown-file-extension ()
  "Recognize Markdown files before a Markdown major mode is active."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/document.MDX")
    (should (org-markdown-preview--markdown-buffer-p))
    (should (org-markdown-preview--supported-buffer-p))))

(ert-deftest org-markdown-preview-test-unsupported-buffer ()
  "Reject buffers which contain neither Org nor Markdown."
  (with-temp-buffer
    (should-not (org-markdown-preview--supported-buffer-p))
    (should-error (org-markdown-preview--refresh-buffer-0)
                  :type 'user-error)))

(provide 'org-markdown-preview-test)
;;; org-markdown-preview-test.el ends here
