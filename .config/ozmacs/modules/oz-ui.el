;;; Code:

;; setting fonts
(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 130
  :weight 'bold)
(set-face-attribute 'variable-pitch nil
  :font "NotoSans"
  :height 130
  :weight 'bold)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 130
  :weight 'bold)
;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono-15"))

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(setq inhibit-startup-message t
      display-line-numbers-type `relative
      truncate-lines t
      confirm-kill-processes nil)


(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open
        dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-projects-backend 'project-el
	dashboard-banner-logo-title "Emacs Is More Than A Text Editor!"
	dashboard-startup-banner (expand-file-name "imgs/logo.png" user-emacs-directory)
	dashboard-center-content t
	dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				      (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
	doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name


(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Sets the default theme to load!!!
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; icons to dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; colorful dired
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

;; disable minor-mode visibility in modeline
(use-package diminish)

;; transparency
(set-frame-parameter nil 'alpha-background 87)
(add-to-list 'default-frame-alist '(alpha-background . 87))

(provide 'oz-ui)
;;; oz-completion.el ends here
