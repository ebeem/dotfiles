;;; oz-git.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-
;;; Commentary:

;;; Code:
(use-package transient)
(use-package magit
  :defer 5
  :init
  ;; Must be set early to prevent ~/.config/emacs/transient from being created
  (setq transient-levels-file  (expand-file-name ".cache/transient/levels" user-emacs-directory)
        transient-values-file  (expand-file-name ".cache/transient/values" user-emacs-directory)
        transient-history-file (expand-file-name ".cache/transient/history" user-emacs-directory)
  		transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package forge
  :after magit
  :commands (forge-create-pullreq forge-create-issue)
  :custom (forge-database-file (expand-file-name ".cache/forge-database.sqlite" user-emacs-directory)))

;; (use-package seq)

;; highlight diffs
;; (use-package diff-hl
;;   :init
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   :config
;;   (global-diff-hl-mode)
;;   (diff-hl-margin-mode))

(provide 'oz-git)
;;; oz-keybindings.el ends here
