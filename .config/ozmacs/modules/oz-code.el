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

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

;; colorize color names / color hex code in buffers. (red) (#ff0000)
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

;; highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; insert some pre-made code by just typing a few characters
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs (list (expand-file-name ".cache/snippets" user-emacs-directory))
        yas--default-user-snippets-dir (expand-file-name ".cache/snippets" user-emacs-directory)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yatemplate
  :after yasnippet)

(use-package treesit-auto
  :demand t
  :init
  (setq treesit-auto-install 'prompt))

(use-package eglot
  :ensure nil
  ;; :hook (before-save . eglot-format-buffer)
  :config
  (defun eglot-csharp-ls-select-solution ()
    (let* ((project (project-current t))
           (files (project-files project))
           (solutions (seq-filter
                       (lambda (file)
                         (string-suffix-p ".sln" file)) files)))
      (if (length> solutions 1)
          (list "-s" (completing-read "Select a solution: " solutions))
        '())))
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-server-programs
        (remove (assoc '(csharp-mode csharp-ts-mode)
                       eglot-server-programs) eglot-server-programs))
  (add-to-list 'eglot-server-programs
               `((csharp-mode csharp-ts-mode)
                 . ,(append '("csharp-ls")
                            (eglot-csharp-ls-select-solution))))
  :commands eglot)

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-session-file (expand-file-name ".cache/lsp-sessions" user-emacs-directory)
;;         lsp-server-install-dir (expand-file-name ".cache/lsp-servers" user-emacs-directory))
;;   :hook (
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package with-venv)

(use-package eldoc-box
  :init
  (setq eldoc-echo-area-use-multiline-p nil)
  :bind
  ([remap eldoc-doc-buffer] . eldoc-box-help-at-point))
  
;; rest client
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

;; import postman requests if needed
(use-package impostman
  :commands (impostman-import-file impostman-import-string))

;; programming langauges major modes
(use-package rjsx-mode
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook (rjsx-mode . eglot-ensure))

(use-package python-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure))

(use-package jupyter
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (jupyter . t))))

(use-package geiser
  :defer t)
(use-package geiser-guile
  :defer t)

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :hook (csharp-mode . eglot-ensure)
  :hook (csharp-ts-mode . eglot-ensure))

(use-package typescript-mode
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook (tsx-ts-mode . eglot-ensure))

(use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode)
  :hook (dart-mode . eglot-ensure))

(use-package flutter
  :mode ("\\.dart\\'" . dart-mode)
  :hook (dart-mode . eglot-ensure))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . eglot-ensure))

(use-package gdscript-mode
  :mode ("\\.gd\\'" . gdscript-ts-mode)
  :hook (gdscript-ts-mode . eglot-ensure)
  :config
  (setq gdscript-gdformat-save-and-format t
        gdscript-use-tab-indents nil
        gdscript-indent-offset 4))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :hook (php-mode . eglot-ensure))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :hook (lua-mode . eglot-ensure))

(use-package csproj-mode
  :mode ("\\.csproj\\'" . csproj-mode)
  :hook (csproj-mode . eglot-ensure))

;; (use-package indent-bars
;;   :ensure (:host github :repo "jdtsmith/indent-bars")
;;   :hook (prog-mode . indent-bars-mode))

;; (use-package pgmacs)

(provide 'oz-code)
;;; oz-code.el ends here
