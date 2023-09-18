;;; Code:

; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(
          ("TODO" . "#ECBE7B")
          ("FIXME" . "#ff6c6b")
          ("REVIEW" . "#46D9FF")
          ("HACK" . "#ECBE7B")
          ("DEPRECATED" . "#c678dd")
          ("NOTE" . "#98be65")
          ("BUG" . "#ff6c6b")))
  :hook (prog-mode . hl-todo-mode))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package editorconfig
  :diminish
  :hook (prog-mode . editorconfig-mode)
  :init)

;; colorize color names / color hex code in buffers. (red) (#ff0000)
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

;; highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; insert some pre-made code by just typing a few characters
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs (expand-file-name ".cache/snippets" user-emacs-directory)
        yas--default-user-snippets-dir (expand-file-name ".cache/snippets" user-emacs-directory)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yatemplate
  :after yasnippet)

(use-package treesit-auto
  :demand t
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package eglot
  :elpaca nil
  :hook (before-save . eglot-format-buffer)
  :config
  (setq eglot-confirm-server-initiated-edits nil))

(use-package eldoc-box
  :config
  (setq eldoc-echo-area-use-multiline-p nil))


;; programming langauges major modes

(use-package rjsx-mode
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook (rjsx-mode . eglot))

(use-package typescript-mode
  :after tree-sitter
  :mode ("\\.tsx\\'" . tsx-ts-mode))

(use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode)
  :hook (dart-mode . eglot))

(use-package flutter
  :mode ("\\.dart\\'" . dart-mode))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode))

(use-package gdscript-mode
  :mode ("\\.gd\\'" . gdscript-mode)
  :hook (dart-mode . eglot))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :hook (dart-mode . eglot))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :hook (dart-mode . eglot))

(provide 'oz-code)
;;; oz-code.el ends here
