;;; Code:
(use-package dashboard
  :ensure t
  :demand t
  :config
  (setq initial-buffer-choice 'dashboard-open
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-banner-logo-title "Emacs Is More Than A Text Editor!"
        dashboard-startup-banner (expand-file-name "imgs/logo.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-items '((projects . 10)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))


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

;;(defun eb/mode-line-meow-state ()
;;  "Return an icon or emoji representing the current Meow state."
;;  (let ((state (meow--current-state)))
;;    (alist-get state
;;               '((normal . "🅝")
;;                 (insert . "🅘")
;;                 (motion . "🅜")
;;                 (keypad . "🅚")
;;                 (beacon . "🅑"))
;;               "?" nil #'eq)))

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

(defun eb/region-info ()
  "Display character, line, and word count for the active region in the mode line."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (chars (abs (- end beg)))
             (lines (count-lines beg end))
             (words (count-words beg end)))
        (format " [%dC/%dW/%dL]" chars words lines))
    ""))

;; (setq mu4e-unread-mail-count 0)
;; (defun eb/mode-line-mu4e-unread-count ()
;;   "Return a string showing the number of unread mu4e messages."
;;   (let* ((output (shell-command-to-string "mu find flag:unread --fields 'n' | wc -l"))
;;          (count (string-to-number (string-trim output))))
;;     (propertize (format " %d  " (/ mu4e-unread-mail-count 2)) 'face 'font-lock-string-face)))

;; (defun eb/setup-mu4e-unread-sync ()
;;   "Set up hooks to keep unread count updated."
;;   (eb/update-mu4e-unread-count) ;; initial load
;;   (add-hook 'mu4e-mark-execute-hook #'eb/update-mu4e-unread-count)
;;   (add-hook 'mu4e-index-updated-hook #'eb/update-mu4e-unread-count))

;; (with-eval-after-load 'mu4e
;;   (eb/setup-mu4e-unread-sync))

(setq-default mode-line-format
 '(" "
   ;; Meow state
   ;; (:eval (eb/mode-line-meow-state))
   ;; " "
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
   "\t"
   (:eval (propertize (eb/region-info)
                      'face 'font-lock-string-face))
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

(use-package spacious-padding
  :ensure t
  :init
  (setq spacious-padding-widths
		'( :internal-border-width 12
		   :header-line-width 4
		   :mode-line-width 6
		   :tab-width 8
		   :right-divider-width 8
		   :scroll-bar-width 8))
  (spacious-padding-mode 1))

(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-alucard t))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :init
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; icons to dired
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; colorful dired
(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

;; colorful dired
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (defun dired-get-size ()
	(interactive)
	(let ((files (dired-get-marked-files)))
      (with-temp-buffer
		(apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
		(message "Size of all marked files: %s"
				 (progn 
                   (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                   (match-string 1)))))))
;; :hook
;; (dired-mode . dired-hide-details-mode))

(provide 'oz-ui)
;;; oz-completion.el ends here
