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

(defun eb/nerd-icon-for-file ()
  "Return a nerd icon based on the current buffer's file type."
  (if (featurep 'nerd-icons)
      (nerd-icons-icon-for-buffer)
    "📁 "))

(defun eb/truncated-file-path ()
  "Return a truncated relative file path like project/m/s/f/file.el."
  (when-let* ((project (project-current))
              (root (expand-file-name (project-root project)))
              (file (or (buffer-file-name) default-directory))
              (relative (file-relative-name file root)))
    (let* ((parts (split-string relative "/"))
           (folders (butlast parts))
           (file-name (car (last parts)))
           (shortened (mapconcat
                       (lambda (s)
                         (let ((first (substring s 0 1)))
                           (if (string-match-p "[^[:alnum:]]" first)
                               (substring s 0 (min 2 (length s)))
                             first)))
                       folders "/"))
           (path (concat (if (string-empty-p shortened)
                             file-name
                           (concat (project-name project)
                                   "/" shortened "/" file-name)))))
      (propertize (concat "📁 " path)
                  'face (when (buffer-modified-p) 'error)))))

(defun eb/meow-state-icon ()
  "Return an icon or emoji representing the current Meow state."
  (let ((state (meow--current-state)))
    (alist-get state
               '((normal . "🅝")
                 (insert . "🅘")
                 (motion . "🅜")
                 (keypad . "🅚")
                 (beacon . "🅑"))
               "?" nil #'eq)))

;; Show current command in modeline
(setq-default
 mode-line-format
 '(" "
   ;; Meow state
   (:eval (eb/meow-state-icon))
   " "
   ;; File icon
   (:eval (eb/nerd-icon-for-file))
   " "
   ;; Shortened path
   (:eval (eb/truncated-file-path))
   "\t"
   ;; Line:Column
   "L%l:%c"
   "\t"
   ;; Percent
   (:eval (propertize "%p%" 'face 'bold))
      ;; 🪟 Right-align major mode + branch
   (:eval
    (let* ((mode-str (format "%s" mode-name))
           (branch-str
            (if (and vc-mode (stringp vc-mode))
                (let ((backend (vc-backend buffer-file-name)))
                  (when backend
                    (concat "🌿 "
                            (substring vc-mode (+ (length backend) 2)))))
              "")) ;; fallback if not a proper vc-mode
           (total-width (+ (length mode-str)
                           (length branch-str)
                           3))) ;; extra space buffer
      (concat
       (propertize " " 'display `((space :align-to (- right-fringe ,total-width))))
       (propertize mode-str 'face 'italic)
       "  "
       branch-str)))
   ))

(message (vc-backend buffer-file-name))
(use-package modus-themes
  :config
  (load-theme 'modus-alucard t))

;; TODO remove github package
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

;; disable minor-mode visibility in modeline
(use-package diminish)

(provide 'oz-ui)
;;; oz-completion.el ends here
