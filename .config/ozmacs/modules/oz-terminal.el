;;; Code:
(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; eshell and evaluation
(use-package eshell
  :ensure nil
  :init
  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh")
        eshell-last-dir-ring-file-name (expand-file-name ".cache/eshell/lastdir" user-emacs-directory)
        eshell-aliases-file (expand-file-name ".cache/eshell/aliases" user-emacs-directory)
        eshell-history-file-name (expand-file-name ".cache/eshell/history" user-emacs-directory))
  (defvar-keymap eb/evaluate-map :doc "EShell/Evaluate")
  (defvar-keymap eb/toggle-map :doc "Toggle")
  :bind-keymap (("C-c e" . eb/evaluate-map))
  :ensure nil
  :bind (
         :map eb/open-map
         ("e" . eshell)
         
         :map eb/toggle-map
         ("e" . eshell-toggle)
         
         :map eb/evaluate-map
         ("b" . eval-buffer)
         ("d" . eval-defun)
         ("e" . eval-expression)
         ("h" . counsel-esh-history)
         ("l" . eval-last-sexp)
         ("r" . eval-region)
         ("s" . eshell)))

(use-package vterm
  :ensure t
  :config
  (setq shell-file-name "/bin/sh"
        vterm-max-scrollback 5000)
    :bind (
         :map eb/open-map
         ("v" . vterm)))

(provide 'oz-terminal)
;;; oz-terminal.el ends here
