;;; Code:

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

  :config
  (defun eb/eshell-git-info ()
	"Return project name and git branch if in a git repo."
	(let ((git-dir (locate-dominating-file default-directory ".git")))
      (when (and git-dir (executable-find "git"))
		(let* ((project-name (file-name-nondirectory (directory-file-name git-dir)))
               ;; Get the current branch using git CLI
               (branch (with-temp-buffer
						 (call-process "git" nil t nil "branch" "--show-current")
						 (string-trim (buffer-string)))))
          ;; Format the output as: [project:branch]
          (concat (propertize " [" 'face 'font-lock-comment-face)
                  (propertize project-name 'face 'font-lock-string-face)
                  (propertize ":" 'face 'font-lock-comment-face)
                  (propertize branch 'face 'font-lock-keyword-face)
                  (propertize "]" 'face 'font-lock-comment-face))))))

  (defun eb/eshell-prompt ()
	"Generate a Zsh-like multi-line Eshell prompt."
	(concat
	 "\n"
	 ;; username and host (user@host)
	 (propertize (user-login-name) 'face 'font-lock-variable-name-face)
	 (propertize "@" 'face 'default)
	 (propertize (system-name) 'face 'font-lock-constant-face)
	 " "
	 ;; current directory (e.g., ~/path/to/dir)
	 (propertize (abbreviate-file-name default-directory) 'face 'font-lock-builtin-face)
	 ;; git project & branch (if applicable)
	 (or (eb/eshell-git-info) "")
	 "\n"
	 ;; input prompt symbol
	 (propertize "❯ " 'face 'success)))

  (with-eval-after-load 'esh-opt
	(setq eshell-prompt-function #'eb/eshell-prompt
          eshell-prompt-regexp "^❯ "))

  (defvar-keymap eb/toggle-map :doc "Toggle")
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

(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package ghostel  
  :ensure t
  :bind (
         :map eb/open-map
         ("g" . ghostel)))

(provide 'oz-terminal)
;;; oz-terminal.el ends here
