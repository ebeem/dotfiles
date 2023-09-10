

;;; Code:
;; reload configurations
(defun utils-reload-init ()
  "Reload configurations and ignore elpaca"
  (interactive)
  (load-file (locate-user-emacs-file "init.el"))
  (ignore (elpaca-process-queues)))

(with-eval-after-load "elpaca-log"
  (setf (alist-get 'utils-reload-init elpaca-log-command-queries) 'silent))
(setq url-configuration-directory (expand-file-name ".cache/url" user-emacs-directory)
      transient-history-file (expand-file-name ".cache/transient/history" user-emacs-directory)
      treesit-extra-load-path '((expand-file-name ".cache/tree-sitter" user-emacs-directory)))
;; delete current file
(defun delete-current-file ()
  "Delete current buffer/file and close the buffer"
  (interactive)
  (progn
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted file: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun copy-this-file
    (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH then open NEW-PATH.\n\nIf FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list
    (read-file-name "Copy file to: ")
    current-prefix-arg))
  (if
      (and buffer-file-name
           (file-exists-p buffer-file-name))
      nil
    (user-error "Buffer is not visiting any file"))
  (let
      ((old-path
        (buffer-file-name
         (buffer-base-buffer)))
       (new-path
        (expand-file-name new-path)))
    (make-directory
     (file-name-directory new-path)
     't)
    (copy-file old-path new-path
               (or force-p 1))
    (find-file new-path)
    (doom-files--update-refs old-path new-path)
    (message "File copied to %S"
             (abbreviate-file-name new-path))))

(use-package sudo-edit)
(use-package visual-fill-column)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq pdf-view-use-scaling t
	pdf-view-display-size 'fit-page
        pdf-view-use-imagemagick nil))

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-list-file-prefix (expand-file-name ".cache/auto-save-list/.saves-" user-emacs-directory))

;; Keep track of recently opened files
(use-package recentf
  :elpaca nil
  :custom (recentf-save-file (expand-file-name ".cache/recentf" user-emacs-directory))
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200))

(use-package savehist
  :elpaca nil
  :custom (savehist-file (expand-file-name ".cache/history" user-emacs-directory)))

(use-package project
  :elpaca nil
  :config
  (setq project-list-file (expand-file-name ".cache/projects" user-emacs-directory)))

(provide 'oz-utilities)
;;; oz-utilities.el ends here
