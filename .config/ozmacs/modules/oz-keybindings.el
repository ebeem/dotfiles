;;; oz-keybindings.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-
;;; Commentary:

;;; Code:
(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer eb/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (eb/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x"))

  (eb/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(project-switch-to-buffer :wk "Project switch buffer")
    "b d" '(kill-this-buffer :wk "Delete buffer")
    "b B" '(switch-to-buffer :wk "Switch buffer")
    "b c" '(clone-indirect-buffer :wk "Clone buffer")
    "b n" '(evil-buffer-new :wk "New buffer")
    "b r" '(revert-buffer :wk "Revert buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(save-buffer :wk "Save buffer")
    "b S" '(save-all-buffers :wk "Save all buffers")
    "b y" '(yank-buffer :wk "Yank buffer")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b M" '(bookmark-delete :wk "Delete bookmark"))

  (eb/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

  (eb/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e s" '(eshell :which-key "Eshell"))

  ;; files
(eb/leader-keys
    "f" '(:ignore t :wk "Files")
    "f u" '(sudo-edit :wk "Sudo edit current file")
    "f U" '(sudo-edit-find-file :wk "Sudo find file")
    "f d" '(delete-current-file :wk "Delete current file")
    "f c" '(copy-this-file :wk "Copy file")
    "f r" '(counsel-recentf :wk "Find recent files"))

  ;; files
(eb/leader-keys
    "g" '(:ignore t :wk "Files")
    "g g" '(magit-status :wk "Magit status"))

 (eb/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '(utils-reload-init :wk "Reload emacs config")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (eb/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (eb/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (eb/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (eb/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dired-jump :wk "Dired")
    "o -" '(dired-jump :wk "Dired")
    "o m" '(mu4e :wk "Mu4e")
    "o n" '(elfeed :wk "Elfeed")
    "o v" '(vterm :wk "Vterm")
    "o e" '(eshell :wk "Eshell"))

  ;; project keybindings
  (eb/leader-keys
    "p" '(:ignore t :wk "Project")
    "p !" '(project-shell-command :wk "Shell command")
    "p &" '(project-async-shell-command :wk "Async shell command")
    "p f" '(project-or-external-find-file :wk "Find file")
    "p g" '(project-or-external-find-regexp :wk "File file regex")
    "p b" '(project-switch-to-buffer :wk "Switch buffer")
    "p c" '(project-compile :wk "Compile")
    "p d" '(project-find-dir :wk "Find directory")
    "p D" '(project-dired :wk "Dired")
    "p e" '(project-eshell :wk "Eshell")
    "p f" '(project-find-file :wk "Find file")
    "p g" '(project-find-regexp :wk "Find file regex")
    "p k" '(project-kill-buffers :wk "Kill buffers")
    "p p" '(project-switch-project :wk "Switch project")
    "p r" '(project-query-replace-regexp :wk "Replace query")
    "p s" '(consult-ripgrep :wk "Search project files")
    "p v" '(project-vc-dir :wk "VC directory")
    "p x" '(project-execute-extended-command :wk "Execute command"))

  ;; search keybindings
  (eb/leader-keys
    "s" '(:ignore t :wk "Search")
    "s p" '(consult-ripgrep :wk "Search project files"))

  (eb/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (eb/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w f" '(make-frame :wk "Create new frame")

    ;; Window splits
    "w d" '(evil-window-delete :wk "Close window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")

    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")))

(defun complete-code ()
  (interactive)
  (if company-mode (company-complete-common)
    (lsp-bridge-popup-complete-menu)))

(global-set-key (kbd "C-SPC") 'complete-code)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(provide 'oz-keybindings)
;;; oz-keybindings.el ends here
