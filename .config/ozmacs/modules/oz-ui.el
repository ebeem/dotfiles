;;; Code:
(use-package dashboard
  :init
  (setq initial-buffer-choice 'dashboard-open
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-banner-logo-title "Emacs Is More Than A Text Editor!"
        dashboard-startup-banner (expand-file-name "imgs/logo.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-items '((projects . 10)
                          (bookmarks . 10)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 35      ;; sets modeline height
;;         doom-modeline-bar-width 5    ;; sets right bar width
;;         doom-modeline-persp-name t   ;; adds perspective name to modeline
;;         doom-modeline-enable-word-count t
;;         doom-modeline-buffer-encoding nil
;;         doom-modeline-buffer-file-name-style 'truncate-with-project
;;         doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(defun eb/mode-line-file-icon ()
  "Return a nerd icon based on the current buffer's file type."
  (if (featurep 'nerd-icons)
      (nerd-icons-icon-for-buffer)
    "📁 "))

(defun eb/mode-line-file-path ()
  "Return a truncated relative file path like project/m/s/f/file.el.
If not in a project, show path from `default-directory`.
If not visiting a file, show buffer name."
  (if-let ((file (buffer-file-name)))
      (let* ((project (project-current))
             (root (if project
                       (expand-file-name (project-root project))
                     default-directory))
             (relative (file-relative-name file root))
             (parts (split-string relative "/"))
             (folders (butlast parts))
             (file-name (car (last parts)))
             (shortened (mapconcat
                         (lambda (s)
                           (let ((first (substring s 0 1)))
                             (if (string-match-p "[^[:alnum:]]" first)
                                 (substring s 0 (min 2 (length s)))
                               first)))
                         folders "/"))
             (path (if (string-empty-p shortened)
                       file-name
                     (if project
                         (concat (project-name project) "/" shortened "/" file-name)
                       (concat shortened "/" file-name)))))
        (propertize path
                    'face (when (buffer-modified-p) 'error)))
    ;; Not visiting a file
    (propertize (buffer-name)
                'face (when (buffer-modified-p) 'error))))

(defun eb/mode-line-meow-state ()
  "Return an icon or emoji representing the current Meow state."
  (let ((state (meow--current-state)))
    (alist-get state
               '((normal . "🅝")
                 (insert . "🅘")
                 (motion . "🅜")
                 (keypad . "🅚")
                 (beacon . "🅑"))
               "?" nil #'eq)))

(defun eb/mode-line-read-only ()
  "Return the a lock icon if the buffer is read-only"
  (when buffer-read-only "🔒 "))

(defun eb/mode-line-mode-name ()
  "Return the major mode name"
  (format "%s" major-mode
          (if (featurep 'nerd-icons)
              (nerd-icons-icon-for-mode major-mode)
            "")))

(defun eb/mode-line-git-branch-name ()
  "Return the current VC branch name as a string, or nil if not under VC."
  (when (and vc-mode buffer-file-name)
    (let ((backend (vc-backend buffer-file-name)))
      (when backend
        (concat " "
                (replace-regexp-in-string
                 "^ Git[:-]" "" vc-mode))))))

(setq-default mode-line-format
 '(" "
   ;; Meow state
   (:eval (eb/mode-line-meow-state))
   " "
   ;; File icon
   (:eval (eb/mode-line-file-icon))
   " "
   ;; Read-only / modified symbol (optional)
   (:eval)
   ;; Shortened path
   (:eval (eb/mode-line-file-path))
   "\t"
   ;; Line:Column
   "L%l:%c"
   "\t"
   ;; Percent
   (:eval (propertize "%p%" 'face 'bold))
   ;; 🪟 Right-align major mode + branch
   (:eval
    (let* ((mode-str (eb/mode-line-mode-name))
           (branch-str (eb/mode-line-git-branch-name))
           (total-width (+ (length mode-str)
                           (length branch-str)
                           3))) ;; extra space buffer
      (concat
       (propertize " " 'display `((space :align-to (- right-fringe ,total-width))))
       mode-str
       "  "
       branch-str)))
   ))

(use-package modus-themes
  :config
  (load-theme 'modus-alucard t)
  (modus-themes-with-colors
    (custom-set-faces
     ;; Add "padding" to the mode lines
     `(mode-line ((,c :underline ,border-mode-line-active
                      :overline ,border-mode-line-active
                      :box (:line-width 4 :color ,bg-mode-line-active))))
     `(mode-line-inactive ((,c :underline ,border-mode-line-inactive
                               :overline ,border-mode-line-inactive
                               :box (:line-width 4 :color ,bg-mode-line-inactive))))))

  ;; ESSENTIAL to make the underline move to the bottom of the box:
  (setq x-underline-at-descent-line t)
  (add-hook 'modus-themes-after-load-theme-hook #'eb/modus-themes-custom-faces))
  
(use-package nerd-icons-completion
  :after marginalia
  :init
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; icons to dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; colorful dired
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

;; colorful dired
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t))
;; :hook
;; (dired-mode . dired-hide-details-mode))

(provide 'oz-ui)
;;; oz-completion.el ends here
