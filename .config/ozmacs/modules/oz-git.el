;;; oz-git.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-
;;; Commentary:

;;; Code:
(use-package magit
  :ensure t
  :defer 5
  :init
  (defvar-keymap eb/magit-map :doc "Magit")
  (setq transient-levels-file  (expand-file-name ".cache/transient/levels" user-emacs-directory)
        transient-values-file  (expand-file-name ".cache/transient/values" user-emacs-directory)
        transient-history-file (expand-file-name ".cache/transient/history" user-emacs-directory)
  		transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-bury-buffer-function #'magit-mode-quit-window)
  
  :bind-keymap (("C-c g" . eb/magit-map))
  :bind (
         :map eb/magit-map
         ("/" . magit-dispatch)
         ("." . magit-file-dispatch)
         ("b" . magit-branch-checkout)
         ("g" . magit-status)
         ("G" . magit-status-here)
         ("D" . magit-file-delete)
         ("B" . magit-blame-addition)
         ("C" . magit-clone)
         ("F" . magit-fetch)
         ("L" . magit-log-buffer-file)
         ("S" . magit-stage-file)
         ("U" . magit-unstage-file)))

(use-package forge
  :after magit
  :ensure t
  :commands (forge-create-pullreq forge-create-issue)
  :custom (forge-database-file (expand-file-name ".cache/forge-database.sqlite" user-emacs-directory)))

;; (use-package seq)

;; highlight diffs
(use-package diff-hl
  :ensure t
  :defer 3
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(provide 'oz-git)
;;; oz-git.el ends here
