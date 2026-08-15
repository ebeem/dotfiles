(use-package project
  :ensure nil
  :config
  ;; TODO: fix function
  (defun eb/project-prune-remembered-files ()
    "Remove non-existent files from `project-remembered-files'."
    (interactive)
    (let* ((proj (project-current))
           (files (project-remembered-files proj))
           (valid-files (seq-filter #'file-exists-p files)))
      (setf (project-remembered-files proj) valid-files)
      (message "Pruned non-existent files from project."))))

;; highlight TODO:FIXME:NOTE:DEPRECATED:HACK:REVIEW:BUG
(use-package mini-hl-todo
  :ensure nil
  :hook (prog-mode . mini-hl-todo-mode))

(use-package editorconfig
  :ensure nil
  :hook (prog-mode . editorconfig-mode))

;; colorize color hex code in buffers. (#ff0000)
(use-package mini-color
  :ensure nil
  :defer t
  :init
  (global-mini-color-mode 1))

;; highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package mini-rainbow-delimiters
  :ensure nil
  :diminish
  :hook (prog-mode . mini-rainbow-delimiters-mode))

;; TODO: remove in emacs 31 and replace by built-in alternative
;; (treesit-auto-install-grammar 'always)
;; (treesit-enabled-modes t)
(use-package treesit-auto
  :ensure t
  :defer t
  :init
  (setq treesit-auto-install 'prompt))

;; TODO: in emacs 31, documentation can be viewed via markdown mode
;; (eglot-documentation-renderer 'markdown-ts-view-mode)
;; (eglot-code-action-indications nil)
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
  ;; (add-to-list 'eglot-server-programs
  ;;              `(scheme-mode . ("guile-lsp-server")))
  (add-to-list 'eglot-server-programs
               `((csharp-mode csharp-ts-mode)
                 . ,(append '("csharp-ls")
                            (eglot-csharp-ls-select-solution))))
  (add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-no-window)))

  :commands eglot
  :bind (("C-c c a" . eglot-code-actions)
         ("C-c c c" . project-recompile)
         ("C-c c d" . eglot-find-typeDefinition)
         ("C-c c D" . eglot-find-implementation)
         ("C-c c F" . eglot-format-buffer)
         ("C-c c f" . eglot-format)
         ("C-c c i" . indent-region)
         ("C-c c I" . eglot-find-implementation)
         ("C-c c j" . eglot-find-declaration)
         ("C-c c k" . eldoc)
         ("C-c c K" . eldoc)
         ("C-c c m" . imenu)
         ("C-c c r" . eglot-rename)
         ("C-c c t" . eglot-find-typeDefinition)
         ("C-c c x" . flymake-show-project-diagnostics)
         ("C-c c ;" . comment-or-uncomment-region)))

(use-package ansi-color
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter))

;; (use-package with-venv)

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p 1))

;; (use-package eldoc-box
;;   :init
;;   (setq eldoc-echo-area-use-multiline-p nil)
;;   :bind
;;   ([remap eldoc-doc-buffer] . eldoc-box-help-at-point))
  
;; rest client
(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode))

;; programming langauges major modes
;; (use-package rjsx-mode
;;   :mode ("\\.jsx\\'" . rjsx-mode)
;;   :hook (rjsx-mode . eglot-ensure))

(use-package javascript-mode
  :ensure nil
  :hook (js-jsx-mode . eglot-ensure))

(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure))

(use-package geiser
  :ensure t
  :defer t
  :init
  (setq geiser-repl-add-project-paths t))

(use-package geiser-guile
  :commands geiser-guile
  :ensure t)

(use-package csharp-mode
  :ensure nil
  :mode ("\\.cs\\'" . csharp-mode)
  :hook (csharp-mode . eglot-ensure)
  :hook (csharp-ts-mode . eglot-ensure))

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.tsx\\'" . rjsx-mode)
  :hook (rjsx-mode . eglot-ensure))

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode))

(use-package gdscript-mode
  :ensure t
  :mode ("\\.gd\\'" . gdscript-ts-mode)
  :hook (gdscript-ts-mode . eglot-ensure)
  :init
  (setq gdscript-gdformat-save-and-format t
		gdscript-use-tab-indents nil)
  (setq gdscript-mode-map
		(let ((map (make-sparse-keymap)))
          ;; Movement
          (define-key map [remap backward-sentence] 'gdscript-nav-backward-block)
          (define-key map [remap forward-sentence] 'gdscript-nav-forward-block)
          (define-key map [remap backward-up-list] 'gdscript-nav-backward-up-list)
          (define-key map [remap mark-defun] 'gdscript-mark-defun)
          (define-key map (kbd "C-c C-f r") 'gdscript-format-region)
          (define-key map (kbd "C-c C-f b") 'gdscript-format-buffer)
          (define-key map (kbd "C-c C-r p") 'gdscript-godot-open-project-in-editor)
          (define-key map (kbd "C-c C-r r") 'gdscript-godot-run-project)
          (define-key map (kbd "C-c C-r d") 'gdscript-godot-run-project-debug)
          (define-key map (kbd "C-c C-r s") 'gdscript-godot-run-current-scene)
          (define-key map (kbd "C-c C-r q") 'gdscript-godot-run-current-scene-debug)
          (define-key map (kbd "C-c C-r e") 'gdscript-godot-edit-current-scene)
          (define-key map (kbd "C-c C-r x") 'gdscript-godot-run-current-script)
          (define-key map (kbd "C-c C-b a") 'gdscript-docs-browse-api)
          (define-key map (kbd "C-c C-b o") 'gdscript-docs-browse-symbol-at-point)
          (define-key map (kbd "C-c C-b s") 'gdscript-docs-online-search-api)
          (define-key map (kbd "C-c C-d C-d s") 'gdscript-debug-display-stack-frame-vars-buffer)
          (define-key map (kbd "C-c C-d C-d d") 'gdscript-debug-display-stack-dump-buffer)
          (define-key map (kbd "C-c C-d C-d b") 'gdscript-debug-display-breakpoint-buffer)
          (define-key map (kbd "C-c C-d C-d i") 'gdscript-debug-display-inspector-buffer)
          (define-key map (kbd "C-c C-d q") 'gdscript-debug-make-server)
          (define-key map (kbd "C-c C-d n") 'gdscript-debug-next)
          (define-key map (kbd "C-c C-d c") 'gdscript-debug-continue)
          (define-key map (kbd "C-c C-d s") 'gdscript-debug-step)
          map)))

(use-package php-ts-mode
  :ensure nil
  :mode ("\\.php\\'" . php-ts-mode)
  :hook (php-ts-mode . eglot-ensure))

(use-package lua-ts-mode
  :ensure nil
  :mode ("\\.lua\\'" . lua-ts-mode)
  :hook (lua-ts-mode . eglot-ensure))

(use-package nxml-mode
  :ensure nil
  :mode ("\\.csproj\\'" . csproj-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package logview
  :custom
  (logview-cache-filename (expand-file-name ".cache/logview-cache.extmap" user-emacs-directory))
  (logview-views-file (expand-file-name ".cache/logview.views" user-emacs-directory))
  :ensure t
  :defer t
  :commands (logview-mode))

(use-package scheme
  :ensure nil
  :config
  ;; custom scheme keywords for gliver
  (put 'define-command 'scheme-indent-function 1)
  (put 'define-var 'scheme-indent-function 1)
  (font-lock-add-keywords 'scheme-mode
						  '(("(\\(define-command\\)\\>\\s-*(?\\([^ \t\n)]+\\)"
							 (1 font-lock-keyword-face)
							 (2 font-lock-function-name-face nil t))))
  (font-lock-add-keywords 'scheme-mode
						  '(("(\\(define-var\\)\\>\\s-*(?\\([^ \t\n)]+\\)"
							 (1 font-lock-keyword-face)
							 (2 font-lock-function-name-face nil t))))

  (defun eb/re-export-all-defs ()
	"Clear the current #:export list and re-export all definitions in the buffer."
	(interactive)
	(let ((orig-point (point-marker)))
      (save-excursion
		(goto-char (point-min))
		(when (search-forward "#:export" nil t)
          (skip-chars-forward " \t\n")
          (when (looking-at "(")
			(let ((start (1+ (point)))
                  (end (save-excursion (forward-list 1) (1- (point)))))
              ;; Delete everything inside the parentheses
              (delete-region start end)))))

      (mark-whole-buffer)
      (call-interactively 'eb/append-defs-to-export)

      (deactivate-mark)
      (goto-char orig-point)
      (set-marker orig-point nil)))

  (defun eb/append-defs-to-export ()
	"Extract defined variables/functions and record types in the active region 
or current line, check if they are already exported, and append new ones to 
the #:export section."
	(interactive)
	(let* ((bounds (if (use-region-p)
                       (cons (region-beginning) (region-end))
					 (cons (line-beginning-position) (line-end-position))))
           (beg (car bounds))
           (end (cdr bounds))
           (name-alist nil)
           (names nil)
           (def-regex "(\\(define\\*?\\(?:-[a-z-]+\\)?\\|def[a-z-]+\\)\\s-+[(]?\\s-*\\([^ \t\n()]+\\)"))
      (save-excursion
		(goto-char beg)
		(while (re-search-forward def-regex end t)
          (unless (nth 8 (syntax-ppss)) 
			(let ((keyword (match-string-no-properties 1)) 
                  (name (match-string-no-properties 2))    
                  (pos (match-beginning 2)))
              ;; ignore 'define-module' and scheme type tags like <container>
              (unless (or (string= keyword "define-module")
                          (string-match-p "^<[^>]+>$" name))
				(push (cons pos name) name-alist))))))

      (save-excursion
		(goto-char beg)
		(while (re-search-forward "(define-record-type\\_>" end t)
          (save-excursion
			(goto-char (match-beginning 0))
			(let ((pos (point)))
              (condition-case nil
                  (let* ((form (read (current-buffer)))
						 (is-record (and (listp form) (eq (car form) 'define-record-type)))
						 (constructor (and is-record (nth 2 form)))
						 (predicate (and is-record (nth 3 form)))
						 (fields (and is-record (nthcdr 4 form))))
					
					(when constructor
                      (cond ((listp constructor)
							 (when (car constructor)
                               (push (cons pos (symbol-name (car constructor))) name-alist)))
							((symbolp constructor)
							 (push (cons pos (symbol-name constructor)) name-alist))))
					
					(when (and predicate (symbolp predicate))
                      (push (cons pos (symbol-name predicate)) name-alist))
					
					(when fields
                      (dolist (field fields)
						(when (listp field)
                          (let ((getter (nth 1 field))
								(setter (nth 2 field)))
							(when (and getter (symbolp getter))
                              (push (cons pos (symbol-name getter)) name-alist))
							(when (and setter (symbolp setter))
                              (push (cons pos (symbol-name setter)) name-alist)))))))
				(error nil))))))

      (setq name-alist (sort name-alist (lambda (a b) (< (car a) (car b)))))
      (setq names (mapcar #'cdr name-alist))
      (setq names (delete-dups names)) ;; remove duplicates between pass 1 and 2

      (if (not names)
          (message "No definitions found in the selected area.")
		(save-excursion
          (goto-char (point-min))
          ;; search for the export section
          (if (search-forward "#:export" nil t)
              (progn
				(skip-chars-forward " \t\n")
				(if (looking-at "(")
					(let* ((export-list-start (point))
                           ;; find the exact end of the export list
                           (export-list-end (save-excursion (forward-list 1) (point)))
                           (names-to-add nil))

                      ;; filter out names that are already exported
                      (dolist (name names)
						(save-excursion
                          (goto-char export-list-start)
                          (let ((sym-regex (concat "\\_<" (regexp-quote name) "\\_>")))
							(unless (re-search-forward sym-regex export-list-end t)
                              (push name names-to-add)))))
                      
                      (setq names-to-add (nreverse names-to-add))

                      ;; insert only the new names
                      (if (not names-to-add)
                          (message "All selected definitions are already exported.")
						(goto-char export-list-end)
						(backward-char 1)
						(dolist (name names-to-add)
                          (newline-and-indent)
                          (insert name))
						
						(message "Successfully exported: %s" (mapconcat #'identity names-to-add ", "))))
                  (message "Expected a list '(' after #:export.")))
			(message "Could not find an #:export section in this file.")))))))

(provide 'oz-code)
;;; oz-code.el ends here


;; TODO:
;; mentor
;; tokei
