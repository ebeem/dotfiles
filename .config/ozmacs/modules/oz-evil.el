;;; oz-evil.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:
(use-package evil
    :init
    (setq evil-want-integration t
          evil-want-keybinding nil
          ;; evil-want-minibuffer t
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

    (defun evil-interactive-shift-right ()
      "vnoremap < <gv"
      (interactive)
      (call-interactively #'evil-shift-right)
      (evil-normal-state)
      (evil-visual-restore))

    (defun evil-interactive-shift-left ()
      "vnoremap > >gv"
      (interactive)
      (call-interactively #'evil-shift-left)
      (evil-normal-state)
      (evil-visual-restore))

    (evil-define-key '(normal) 'global
      "gcn" 'flymake-goto-next-error
      "gcp" 'flymake-goto-prev-error)

    (unbind-key (kbd "C-k") evil-insert-state-map)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (setq-default evil-shift-width 2)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (define-key evil-ex-completion-map (kbd "C-j") 'next-complete-history-element)
    (define-key evil-ex-completion-map (kbd "C-k") 'previous-complete-history-element)

    (define-key evil-visual-state-map (kbd ">") 'evil-interactive-shift-right)
    (define-key evil-visual-state-map (kbd "<") 'evil-interactive-shift-left))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map
    "gz" nil))

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
  (global-evil-mc-mode 1)
  (evil-define-key '(normal) 'global
    "gzd" 'evil-mc-make-and-goto-next-match
    "gzD" 'evil-mc-make-and-goto-prev-match
    "gzs" 'evil-mc-skip-and-goto-next-match
    "gzS" 'evil-mc-skip-and-goto-prev-match
    "gzc" 'evil-mc-skip-and-goto-next-cursor
    "gzC" 'evil-mc-skip-and-goto-prev-cursor
    "gzj" 'evil-mc-make-cursor-move-next-line
    "gzk" 'evil-mc-make-cursor-move-prev-line
    "gzm" 'evil-mc-make-all-cursors
    "gzn" 'evil-mc-make-and-goto-next-cursor
    "gzN" 'evil-mc-make-and-goto-last-cursor
    "gzp" 'evil-mc-make-and-goto-prev-cursor
    "gzP" 'evil-mc-make-and-goto-first-cursor
    "gzq" 'evil-mc-undo-all-cursors
    "gzt" '+multiple-cursors/evil-mc-toggle-cursors
    "gzu" '+multiple-cursors/evil-mc-undo-cursor
    "gzz" '+multiple-cursors/evil-mc-toggle-cursor-here))

(use-package evil-nerd-commenter
  :after evil
  :config
    (evil-define-key '(visual) 'global
      "gc" 'evilnc-comment-or-uncomment-lines)

    (evil-define-key '(normal) 'global
      "gcc" 'evilnc-comment-or-uncomment-lines))

;; extra 'evil-collection' bindings
(defun eb/evil-keybindings-hook (mode mode-keymaps &rest _rest)
  (when (equal mode 'dired)
    (evil-define-key 'normal dired-mode-map
      "o" 'dired-open-file)))

(provide 'oz-evil)
;;; oz-evil.el ends here
