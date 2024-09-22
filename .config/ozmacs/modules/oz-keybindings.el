;;; oz-keybindings.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-
;;; Commentary:

;;; Code:
(use-package general
  :after evil-collection
  :init
  (general-auto-unbind-keys)
  :config
  (general-evil-setup)

  (evil-define-key '(visual) 'global
    "gc" 'evilnc-comment-or-uncomment-lines)

  (evil-define-key '(normal) 'global
    "gcc" 'evilnc-comment-or-uncomment-lines
    "gcn" 'flycheck-next-error
    "gcp" 'flycheck-previous-error)

  (evil-collection-define-key 'normal 'emacs-lisp-mode-map
    "gz" nil)

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
    "gzz" '+multiple-cursors/evil-mc-toggle-cursor-here)

  (evil-define-key '(visual) 'global
    ">" 'evil-interactive-shift-right
    "<" 'evil-interactive-shift-left)


  ;; set up 'SPC' as the global leader key
  (general-create-definer eb/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; (eb/leader-keys
  ;;   "SPC" '(embark :wk "Embark"))

  (eb/leader-keys
    ":" '(eval-expression :wk "Evaluate")
    ";" '(execute-extended-command :wk "Commands")
    "x" '(scratch-buffer :wk "Scratch Buffer")
    "C" '(org-capture :wk "Org Capture")
    ">" '(next-buffer :wk "Next buffer")
    "<" '(previous-buffer :wk "Previous buffer"))

  (eb/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(project-switch-to-buffer :wk "Project switch buffer")
    "b B" '(switch-to-buffer :wk "Switch buffer")
    "b c" '(clone-indirect-buffer :wk "Clone buffer")
    "b d" '(kill-this-buffer :wk "Kill buffer")
    "b h" '(previous-buffer :wk "Previous buffer")
    "b k" '(kill-this-buffer :wk "Kill buffer")
    "b l" '(next-buffer :wk "Next buffer")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b M" '(bookmark-delete :wk "Delete bookmark")
    "b n" '(evil-buffer-new :wk "New buffer")
    "b p" '(paste-buffer :wk "Paste buffer")
    "b r" '(revert-buffer :wk "Revert buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(save-buffer :wk "Save buffer")
    "b S" '(save-all-buffers :wk "Save all buffers")
    "b x" '(scratch-buffer :wk "Scratch Buffer")
    "b y" '(yank-buffer :wk "Yank buffer"))

  ;; (eb/leader-keys
  ;;   "c" '(:ignore t :wk "LSP Code")
  ;;   "c a" '(lsp-execute-code-action :wk "LSP Execute code action")
  ;;   "c c" '(compile :wk "Compile")
  ;;   "c C" '(recompile :wk "Recompile")
  ;;   "c d" '(lsp-find-type-definition :wk "Jump to definition")
  ;;   "c D" '(lsp-find-implementation :wk "Jump to references")
  ;;   "c F" '(lsp-format-buffer :wk "Format buffer/region")
  ;;   "c f" '(lsp-format-region :wk "Format buffer/region")
  ;;   "c i" '(imenu :wk "Imenu")
  ;;   "c I" '(lsp-find-implementation :wk "Find implementations")
  ;;   "c j" '(lsp-find-declaration :wk "LSP Find declaration")
  ;;   "c k" '(eldoc-box-help-at-point :wk "Jump to documentation")
  ;;   "c K" '(eldoc :wk "Jump to documentation")
  ;;   "c r" '(lsp-rename :wk "LSP Rename")
  ;;   "c t" '(lsp-find-type-definition :wk "Find type definition")
  ;;   "c x" '(flycheck-list-errors :wk "List errors"))

  (eb/leader-keys
    "c" '(:ignore t :wk "LSP Code")
    "c a" '(eglot-code-actions :wk "LSP Execute code action")
    "c c" '(compile :wk "Compile")
    "c C" '(recompile :wk "Recompile")
    "c d" '(eglot-find-typeDefinition :wk "Jump to definition")
    "c D" '(eglot-find-implementation :wk "Jump to references")
    "c F" '(eglot-format-buffer :wk "Format buffer/region")
    "c f" '(eglot-format :wk "Format buffer/region")
    "c i" '(imenu :wk "Imenu")
    "c I" '(eglot-find-implementation :wk "Find implementations")
    "c j" '(eglot-find-declaration :wk "LSP Find declaration")
    "c k" '(eldoc-box-help-at-point :wk "Jump to documentation")
    "c K" '(eldoc :wk "Jump to documentation")
    "c r" '(eglot-rename :wk "LSP Rename")
    "c t" '(eglot-find-typeDefinition :wk "Find type definition")
    "c x" '(flycheck-list-errors :wk "List errors"))

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
    "f c" '(editorconfig-find-current-editorconfig :wk "Editor config")
    "f c" '(copy-this-file :wk "Copy file")
    "f u" '(sudo-edit :wk "Sudo edit current file")
    "f U" '(sudo-edit-find-file :wk "Sudo find file")
    "f d" '(delete-current-file :wk "Delete current file")
    "f f" '(find-file :wk "File file")
    "f r" '(counsel-recentf :wk "Find recent files"))

  ;; git
  (eb/leader-keys
    "g" '(:ignore t :wk "Git")
    "g R" '(vc-revert :wk "Revert file")
    "g /" '(magit-dispatch :wk "Magit dispatch")
    "g ." '(magit-file-dispatch :wk "Magit file dispatch")
    "g '" '(forge-dispatch :wk "Forge dispatch")
    "g b" '(magit-branch-checkout :wk "Magit switch branch")
    "g g" '(magit-status :wk "Magit status")
    "g G" '(magit-status-here :wk "Magit status here")
    "g D" '(magit-file-delete :wk "Magit file delete")
    "g B" '(magit-blame-addition :wk "Magit blame")
    "g C" '(magit-clone :wk "Magit clone")
    "g F" '(magit-fetch :wk "Magit fetch")
    "g L" '(magit-log-buffer-file :wk "Magit buffer log")
    "g S" '(magit-stage-file :wk "Git stage file")
    "g U" '(magit-unstage-file :wk "Git unstage file")

    "g f" '(:ignore t :wk "Git files")
    "g f f" '(magit-find-file :wk "Find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g f c" '(magit-show-commit :wk "Find commit")
    "g f i" '(forge-visit-issue :wk "Find issue")
    "g f p" '(forge-visit-pullreq :wk "Find pull request")

    "g i" '(:ignore t :wk "Git browse")
    "g i r" '(forge-browse-remote :wk "Browse remote")
    "g i c" '(forge-browse-commit :wk "Browse commit")
    "g i i" '(forge-browse-issue :wk "Browse an issue")
    "g i p" '(forge-browse-pullreq :wk "Browse a pull request")
    "g i I" '(forge-browse-issues :wk "Browse issues")
    "g i P" '(forge-browse-pullreqs :wk "Browse pull requests")

    "g l r" '(magit-list-repositories :wk "List repositories")
    "g l s" '(magit-list-submodules :wk "List submodules")
    "g l i" '(forge-list-issues :wk "List issues")
    "g l p" '(forge-list-pullreqs :wk "List pull requests")
    "g l n" '(forge-list-notifications :wk "List notifications")

    "g c" '(:ignore t :wk "Git create")
    "g c r" '(magit-init :wk "Initialize repo")
    "g c R" '(magit-clone :wk "Clone repo")
    "g c c" '(magit-commit-create :wk "Commit")
    "g c f" '(magit-commit-fixup :wk "Fixup")
    "g c b" '(magit-branch-and-checkout :wk "Branch")
    "g c i" '(forge-create-issue :wk "Issue")
    "g c p" '(forge-create-pullreq :wk "Pull request"))


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
    "n" '(:ignore t :wk "Org")
    "n a" '(org-agenda :wk "Org agenda")
    "n e" '(org-export-dispatch :wk "Org export dispatch")
    "n i" '(org-toggle-item :wk "Org toggle item")
    "n t" '(org-todo :wk "Org todo")
    "n B" '(org-babel-tangle :wk "Org babel tangle")
    "n T" '(org-todo-list :wk "Org todo list")
    "n b" '(:ignore t :wk "Tables")
    "n b -" '(org-table-insert-hline :wk "Insert hline in table")
    "n r" '(:ignore t :wk "Org Roam")
    "n r c" '(org-roam-capture :wk "Org roam capture")
    "n d" '(:ignore t :wk "Date/deadline")
    "n d t" '(org-time-stamp :wk "Org time stamp"))

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
    "p s" '(consult-grep :wk "Search project files")
    "p S" '(project-find-regexp :wk "Search project files")
    "p v" '(project-vc-dir :wk "VC directory")
    "p x" '(project-execute-extended-command :wk "Execute command"))

  ;; search keybindings
  (eb/leader-keys
    "s" '(:ignore t :wk "Search/Spelling")
    "s p" '(consult-ripgrep :wk "Search project files")
    "s c" '(jinx-correct :wk "Correct word"))

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

    ;; TODO: extend from 'evil-window-map'
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
 (when (boundp 'corfu-mode)
     (completion-at-point))
 (when (boundp 'company-mode)
     (company-complete-common)))

;; (global-set-key (kbd "C-SPC") 'completion-at-point)
(global-set-key (kbd "C-SPC") 'complete-code)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "TAB") 'self-insert-command)

(provide 'oz-keybindings)
;;; oz-keybindings.el ends here
