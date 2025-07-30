;;; Code:

;; font
(set-face-attribute 'default nil
                    :font "Iosevka"
                    ;; :height (/ (display-pixel-width) 13)
                    :height 165
                    ;; :height 200
                    :weight 'demibold)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    ;; :height (/ (display-pixel-width) 13)
                    :height 165
                    ;; :height 200
                    :weight 'demibold)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    ;; :height (/ (display-pixel-width) 13)
                    :height 165
                    ;; :height 200
                    :weight 'bold)

(setq-default truncate-lines t
              word-wrap t
              inhibit-startup-message t
              display-line-numbers-type `relative
              global-visual-line-mode t
              confirm-kill-processes nil
              fill-column 80
              indent-tabs-mode t
              standard-indent 4
              typescript-ts-mode-indent-offset 4
              tab-width 4
              undo-limit 1000
              auto-save-default t
              tab-always-indent t
              undo-no-redo t
              sentence-end-double-space nil			;; avoid whitespaces
              scroll-conservatively 1000			;; no cursor jumping on scrolling
              delete-by-moving-to-trash t			;; move deleted files to trash
              undo-limit 67108864
			  undo-strong-limit 100663296
			  undo-outer-limit 1006632960
			  ispell-program-name "aspell"
			  ispell-extra-args '("--sug-mode=ultra" "--lang=en")
              backup-directory-alist '((".*" . "~/.Trash")))

(put 'downcase-region 'disabled nil)

;; (add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))
;; (add-hook 'before-save-hook #'whitespace-cleanup)	;; cleanup whitespaces

(global-auto-revert-mode 1)							;; auto revert unchanged buffers
(column-number-mode)								;; modeline column indicator
(delete-selection-mode 1)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name ".cache" user-emacs-directory) t)
(make-directory (expand-file-name ".cache/auto-saves/" user-emacs-directory) t)
(setq read-process-output-max (* 1024 1024))

(setq url-configuration-directory (expand-file-name ".cache/url" user-emacs-directory)
      transient-history-file (expand-file-name ".cache/transient/history" user-emacs-directory)
      eshell-directory-name (expand-file-name ".cache/eshell" user-emacs-directory)
      bookmark-default-file (expand-file-name ".cache/bookmarks.bmk" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name ".cache/auto-save-list/.saves-" user-emacs-directory)
      create-lockfiles nil
      auto-save-list-file-prefix (expand-file-name ".cache/auto-saves/sessions" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name ".cache/auto-saves/" user-emacs-directory) t))
      backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(add-to-list 'treesit-extra-load-path (expand-file-name ".cache/tree-sitter" user-emacs-directory))

;; avoid storing custom lines in 'init.el' file
(setq custom-file (expand-file-name ".cache/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; transparency
(set-frame-parameter nil 'alpha-background 92)
(add-to-list 'default-frame-alist '(alpha-background . 92))

;; tramp fix remote paths
(use-package tramp
  :ensure nil
  :init
  (setq tramp-persistency-file-name  (expand-file-name ".cache/tramp" user-emacs-directory))
  :config
  (setq tramp-remote-path
      (append tramp-remote-path
              '(tramp-own-remote-path))))

;; keybindings
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
(provide 'oz-settings)
;;; oz-settings.el ends here
