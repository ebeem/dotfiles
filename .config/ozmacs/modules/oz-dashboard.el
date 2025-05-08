(defvar simple-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'simple-dashboard-visit-project)
    map)
  "Keymap for `simple-dashboard-mode`.")

(define-derived-mode simple-dashboard-mode special-mode "SimpleDashboard"
  "Major mode for the simple Emacs dashboard.")

(defun simple-dashboard-recent-projects ()
  "Get a list of recent projects using project.el."
  (let ((projects (seq-take (project-known-project-roots) 5)))
    (if projects
        projects
      '("No recent projects"))))

(defun simple-dashboard-visit-project ()
  "Open the project at point using project.el."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (and line
               (string-match "/\\(.*\\)" line)) ;; match a path-like string
      (let ((project-path (string-trim (substring line (match-beginning 0)))))
        (when (file-directory-p project-path)
          (project-switch-project project-path))))))

(defun simple-dashboard-print ()
  "Display a minimal dashboard with project.el and nerd-icons."
  (interactive)
  (let* ((welcome (propertize "🧠 Welcome back, mortal."
                              'face '(:height 1.5 :weight bold)))
         (project-icon (nerd-icons-icon-for-dir "/home")) ; generic folder icon
         (projects (simple-dashboard-recent-projects))
         (package-count (length package-activated-list))
         (startup-time (format "%.2f" (float-time (time-subtract after-init-time before-init-time))))
         (pkg-icon (nerd-icons-mdicon "nf-md-package_variant"))
         (time-icon (nerd-icons-mdicon "nf-md-timer_sand")))

    (with-current-buffer (get-buffer-create "*dashboard*")
      (read-only-mode -1)
      (erase-buffer)
      (simple-dashboard-mode)
      (insert welcome)
      (insert "\n\nRecent Projects:\n")
      (dolist (p projects)
        (insert (format "  %s %s\n" project-icon (abbreviate-file-name p))))
      (insert (format "\n%s Packages loaded: %d\n" pkg-icon package-count))
      (insert (format "%s Startup time: %s seconds\n" time-icon startup-time))
      (goto-char (point-min))
      (read-only-mode 1)
      (switch-to-buffer (current-buffer)))))

;; Show dashboard after Emacs startup
(add-hook 'emacs-startup-hook #'simple-dashboard-print)

(provide 'oz-dashboard)
;;; oz-dashboard.el ends here
