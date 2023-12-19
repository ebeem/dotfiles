;;; oz-evil.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:
(use-package evil
    :init
    (setq evil-want-integration t
          evil-want-keybinding nil
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-want-C-u-scroll t
          evil-want-C-i-jump nil
          evil-respect-visual-line-mode t
          evil-kill-on-visual-paste nil
          evil-undo-system 'undo-tree)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-normal-state-map (kbd "K") 'eldoc-box-help-at-point)

    (unbind-key (kbd "C-k") evil-insert-state-map)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (setq-default evil-shift-width 2)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (define-key evil-ex-completion-map (kbd "C-j") 'next-complete-history-element)
    (define-key evil-ex-completion-map (kbd "C-k") 'previous-complete-history-element))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  (use-package evil-escape
    :after evil
    :config
    (setq-default evil-escape-key-sequence "jk"
          evil-escape-excluded-states '(normal visual multiedit emacs motion)
          evil-escape-delay 0.2)
    (evil-escape-mode))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))


(use-package evil-nerd-commenter
  :after evil)

;; extra 'evil-collection' bindings
(defun eb/evil-keybindings-hook (mode mode-keymaps &rest _rest)
  (when (equal mode 'dired)
    (evil-define-key 'normal dired-mode-map
      "o" 'dired-open-file)))

(provide 'oz-evil)
;;; oz-evil.el ends here
