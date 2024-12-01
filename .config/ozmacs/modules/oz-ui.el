;;; Code:
(use-package dashboard
  :init
  (setq initial-buffer-choice 'dashboard-open
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-banner-logo-title "Emacs Is More Than A Text Editor!"
        dashboard-startup-banner (expand-file-name "imgs/logo-dragon.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-items '((projects . 10)
                          (bookmarks . 10)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                      (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package modus-themes
  :config
  (load-theme 'modus-alucard t))

;; TODO remove github package
(use-package nerd-icons-completion
  :after marginalia
  :init
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

;; colorful dired
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t))
  ;; :hook
  ;; (dired-mode . dired-hide-details-mode))

;; disable minor-mode visibility in modeline
(use-package diminish)
(use-package keycast)

(provide 'oz-ui)
;;; oz-completion.el ends here
