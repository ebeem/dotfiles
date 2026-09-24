;;; Code:  -*- lexical-binding: t; -*-
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
   ;; Read-only indicator
   (:eval (eb/mode-line-read-only))
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

(use-package mini-padding
  :ensure nil
  :defer t
  :hook (emacs-startup . mini-padding-mode)
  :init
  (setq mini-padding-widths
		'( :internal-border-width 12
		   :mode-line-width       6
		   :fringe-width          6
		   :right-divider-width   8
		   :scroll-bar-width      8)))

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
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (defun dired-get-size ()
	(interactive)
	(let ((files (dired-get-marked-files)))
      (with-temp-buffer
		(apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
		(message "Size of all marked files: %s"
				 (progn 
                   (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                   (match-string 1))))))

  (defun eb/mount-and-open-in-dired ()
	"List unmounted drives with their labels, mount the selection, and open Dired."
	(interactive)
	(let* ((cmd "lsblk -Pp -o NAME,SIZE,TYPE,FSTYPE,LABEL,MOUNTPOINT")
           (lines (split-string (shell-command-to-string cmd) "\n" t))
           (candidates nil))
      ;; parse lsblk key="value" output
      (dolist (line lines)
		(let ((pos 0)
              (data nil))
          (while (string-match "\\([A-Z_]+\\)=\"\\([^\"]*\\)\"" line pos)
			(push (cons (match-string 1 line) (match-string 2 line)) data)
			(setq pos (match-end 0)))
          (let ((name       (cdr (assoc "NAME" data)))
				(size       (cdr (assoc "SIZE" data)))
				(fstype     (cdr (assoc "FSTYPE" data)))
				(label      (cdr (assoc "LABEL" data)))
				(mountpoint (cdr (assoc "MOUNTPOINT" data))))
			;; filter for unmounted block devices with a filesystem (skipping swap)
			(when (and name
                       (not (equal name ""))
                       (or (null mountpoint) (equal mountpoint ""))
                       fstype
                       (not (equal fstype ""))
                       (not (equal fstype "swap")))
              (let* ((lbl (if (and label (not (equal label "")))
                              (format "[%s]" label)
							"[no label]"))
					 (display (format "%-14s  %-18s  (%s, %s)" name lbl size fstype)))
				(push (cons display name) candidates))))))
      (if (null candidates)
          (message "No unmounted drives found.")
		(let* ((cand-alist (reverse candidates))
               (choice (completing-read "Mount drive: " cand-alist nil t))
               (device (cdr (assoc-string choice cand-alist))))
          (if (not device)
              (message "Failed to resolve block device for choice: %s" choice)
			(message "Mounting %s..." device)
			(let* ((mount-cmd (format "udisksctl mount -b %s" (shell-quote-argument device)))
                   (output (string-trim (shell-command-to-string mount-cmd))))
              ;; extract the mount directory from udisksctl output, or query lsblk directly
              (let ((mountpoint
					 (or (when (string-match "at \\(/[^. \n\t]+\\(?: [^. \n\t]+\\)*\\)" output)
                           (match-string 1 output))
						 (string-trim
                          (shell-command-to-string
                           (format "lsblk -no MOUNTPOINT %s" (shell-quote-argument device)))))))
				(if (and mountpoint (not (string-empty-p mountpoint)) (file-directory-p mountpoint))
					(progn
                      (message "Mounted at: %s" mountpoint)
                      (dired mountpoint))
                  (message "Mount failed: %s" output))))))))))

;; colorful dired
(use-package mini-diredfl
  :ensure nil
  :hook
  (dired-mode . mini-diredfl-mode))

(provide 'oz-ui)
;;; oz-ui.el ends here
