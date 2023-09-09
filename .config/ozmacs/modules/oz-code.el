

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

(use-package
  :elpaca nil
  :init (global-tree-sitter-mode))

(use-package lsp-bridge
  :elpaca (:host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  :init
  (global-lsp-bridge-mode)
  :hook (acm-mode . (lambda () (company-mode -1)))
  :config
  (add-to-list 'lsp-bridge-completion-stop-commands #'evil-escape)
  :bind
  (:map acm-mode-map
      ("C-j" . acm-select-next)
      ("C-k" . acm-select-prev)
      ("C-M-j" . acm-select-next-page)
      ("C-M-k" . acm-select-prev-page))))

;; programming langauges major modes
;;(use-package elisp-mode)
;;(use-package csharp-mode)

(use-package rjsx-mode)
(use-package typescript-mode)
(use-package lsp-java)
(use-package dart-mode)
(use-package flutter)
(use-package csv-mode)
(use-package gdscript-mode)

(provide 'oz-code)
;;; oz-code.el ends here
