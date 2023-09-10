

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

;; colorize color names / color hex code in buffers. (red) (#ff0000)
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

;; highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet)

(use-package treesit-auto
  :demand t
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package eglot
  :elpaca nil)

;; programming langauges major modes

(use-package rjsx-mode
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook (rjsx-mode . eglot))

(use-package typescript-mode
  :after tree-sitter
  :mode ("\\.tsx\\'" . typescript-ts-mode)
  :config
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

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
