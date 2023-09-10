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
          evil-undo-system 'undo-redo)
    :config
    (evil-mode)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (unbind-key (kbd "C-k") evil-insert-state-map )

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (setq-default evil-shift-width 2)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (define-key evil-ex-completion-map (kbd "C-j") 'next-complete-history-element)
    (define-key evil-ex-completion-map (kbd "C-k") 'previous-complete-history-element)

    (define-key evil-normal-state-map "gzmj" 'evil-next-visual-line))


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

;; file opening procedures
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

;; extra 'evil-collection' bindings
(defun eb/evil-keybindings-hook (mode mode-keymaps &rest _rest)
  (when (equal mode 'dired)
    (evil-define-key 'normal dired-mode-map
      "o" 'dired-open-file)))

;; doom's escape hook
(defun eb/escape (&optional interactive)
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
	  ;; quit the minibuffer if open.
	  (when interactive
	    (setq this-command 'abort-recursive-edit))
	  (abort-recursive-edit))
	  ;; Run all escape hooks. If any returns non-nil, then stop there.
	  ((run-hook-with-args-until-success 'keyboard-escape-hook))
	  ;; don't abort macros
	  ((or defining-kbd-macro executing-kbd-macro) nil)
	  ;; Back to the default
	  ((unwind-protect (keyboard-quit)
	    (when interactive
	      (setq this-command 'keyboard-quit))))))

(add-hook 'evil-collection-setup-hook #'eb/evil-keybindings-hook)
(global-set-key [remap keyboard-quit] #'eb/escape)

(defun eb/escape-multiple-cursors ()
   "Clear evil-mc cursors and restore state."
   (when (evil-mc-has-cursors-p)
   	(evil-mc-undo-all-cursors)
   	(evil-mc-resume-cursors)
        t))
(add-hook 'keyboard-escape-hook 'eb/escape-multiple-cursors)

(provide 'oz-evil)
;;; oz-evil.el ends here
