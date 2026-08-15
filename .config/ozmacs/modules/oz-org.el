;;; Code:
(use-package org-mode
  :ensure nil
  :init
  (which-key-add-key-based-replacements "C-c n" "Note")
  :hook (org-mode . org-indent-mode)
  :config
  (defun eb/org-insert-image ()
	"Prompt for a file and insert it as an Org image link at point."
	(interactive)
	(let* ((image-file (read-file-name "Select image: "))
           ;; Convert to a relative path so your Org file is portable
           (relative-path (file-relative-name image-file default-directory))
           ;; Format it using Org's link syntax
           (org-link (format "[[file:%s]]\n" relative-path)))
      (insert org-link)
      (when (eq major-mode 'org-mode)
		(org-display-inline-images))))
  :bind (("C-c n a" . org-agenda)
         ("C-c n b" . org-babel-tangle)
         ("C-c n e" . org-export-dispatch)
         ("C-c n i" . org-toggle-item)
         ("C-c n t" . org-todo)
         ("C-c n T" . org-todo-list)
         ("C-c n s" . consult-org-heading)
         ("C-c n d" . org-time-stamp)))

(use-package denote
  :ensure t
  :bind (("C-c n f" . denote-open-or-create)))

(use-package org-faces
  :init
  (setq org-hide-emphasis-markers t)
  (set-face-attribute 'org-document-title nil :inherit 'fixed-pitch :weight 'bold :height 1.2)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-issue-message-flag nil))

(provide 'oz-org)
;;; oz-org.el ends here
