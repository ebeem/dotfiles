;;; Code:

;; font
(defun eb/reset-font ()
  "Reset the font to default ones."
  (interactive)
  (set-face-attribute 'default nil
					  :font "Iosevka Charon Mono"
					  :height 165
					  :weight 'bold)
  (set-face-attribute 'fixed-pitch nil
					  :font "Iosevka Charon Mono"
					  :height 165
					  :weight 'bold)
  (set-face-attribute 'variable-pitch nil
					  :font "Iosevka Charon Mono"
					  :height 165
					  :weight 'bold))

(defun eb/reset-font-hook (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  (eb/reset-font))

;; Run for the first frame (if it exists) and all subsequent frames
(add-hook 'after-make-frame-functions #'eb/reset-font-hook)
(eb/reset-font)

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
              auto-save-default t
              tab-always-indent t
              undo-no-redo t
              sentence-end-double-space nil			;; avoid whitespaces
              scroll-conservatively 1000			;; no cursor jumping on scrolling
              delete-by-moving-to-trash t			;; move deleted files to trash
              undo-limit 67108864
			  undo-strong-limit 100663296
			  undo-outer-limit 1006632960
			  resize-mini-windows nil
			  ispell-program-name "aspell"
			  ispell-extra-args '("--sug-mode=ultra" "--lang=en")
)

;; completion configuration (completion-styles overridden by orderless in oz-completion)
(setq-default completion-auto-select t ;; Show completion on first call
      completion-auto-help 'visible ;; Display *Completions* upon first request
      completions-format 'one-column ;; Use only one column
      completions-sort 'historical ;; Order based on minibuffer history
      completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
      completion-ignore-case t)

(put 'downcase-region 'disabled nil)

;; (add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'prog-mode-hook
		  (lambda ()
			(display-line-numbers-mode)
			(electric-pair-local-mode)))
;; (add-hook 'before-save-hook #'whitespace-cleanup)	;; cleanup whitespaces

(global-auto-revert-mode 1)							;; auto revert unchanged buffers
(column-number-mode)								;; modeline column indicator
(delete-selection-mode 1)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name ".cache" user-emacs-directory) t)
(make-directory (expand-file-name ".cache/auto-saves/" user-emacs-directory) t)
(setq url-configuration-directory (expand-file-name ".cache/url" user-emacs-directory)
      transient-history-file (expand-file-name ".cache/transient/history" user-emacs-directory)
      eshell-directory-name (expand-file-name ".cache/eshell" user-emacs-directory)
      bookmark-default-file (expand-file-name ".cache/bookmarks.bmk" user-emacs-directory)
      create-lockfiles nil
      auto-save-list-file-prefix (expand-file-name ".cache/auto-saves/sessions" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name ".cache/auto-saves/" user-emacs-directory) t))
      backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(add-to-list 'treesit-extra-load-path (expand-file-name ".cache/tree-sitter" user-emacs-directory))

;; load custom file (path already set in early-init.el)
(when (file-exists-p custom-file)
  (load custom-file))

;; transparency
(set-frame-parameter nil 'alpha-background 97)
(add-to-list 'default-frame-alist '(alpha-background . 97))

;; tramp fix remote paths
(use-package tramp
  :ensure nil
  :init
  (setq tramp-persistency-file-name  (expand-file-name ".cache/tramp" user-emacs-directory))
  :config
  (setq tramp-remote-path
      (append tramp-remote-path
              '(tramp-own-remote-path))))

;; (use-package easysession
;;   :ensure t
;;   :demand t
;;   :init
;;   (setq easysession-directory (expand-file-name ".cache/easy-session" user-emacs-directory))
;;   :custom
;;   (easysession-save-interval (* 10 60))
;;   (easysession-switch-to-save-session t)
;;   (easysession-switch-to-exclude-current t)
;;   (easysession-save-mode-lighter-show-session-name t)
;;   :config
;;   (global-set-key (kbd "C-c sl") #'easysession-switch-to) ; Load session
;;   (global-set-key (kbd "C-c ss") #'easysession-save) ; Save session
;;   (global-set-key (kbd "C-c sL") #'easysession-switch-to-and-restore-geometry)
;;   (global-set-key (kbd "C-c sr") #'easysession-rename)
;;   (global-set-key (kbd "C-c sR") #'easysession-reset)
;;   (global-set-key (kbd "C-c su") #'easysession-unload)
;;   (global-set-key (kbd "C-c sd") #'easysession-delete)
;;   (setq easysession-setup-load-session t)
;;   (easysession-setup))

;; keybindings
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)

(provide 'oz-settings)
;;; oz-settings.el ends here
