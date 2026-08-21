;;; mini-logview.el --- Structured log viewer with level filtering -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup mini-logview nil
  "Structured log file viewer."
  :group 'tools)

(defface mini-logview-timestamp-face
  '((t :foreground "#c6a0f6"))
  "Face for timestamps."
  :group 'mini-logview)

(defface mini-logview-thread-face
  '((t :foreground "#f5bde6" :weight bold))
  "Face for thread names."
  :group 'mini-logview)

(defface mini-logview-logger-face
  '((t :foreground "#f5a97f"))
  "Face for source files/loggers."
  :group 'mini-logview)

(defface mini-logview-pointer-face
  '((t :foreground "#a6da95" :weight bold))
  "Face for pointers and object references."
  :group 'mini-logview)

(defface mini-logview-coords-face
  '((t :foreground "#a6da95"))
  "Face for dimensions and coordinates."
  :group 'mini-logview)

(defface mini-logview-trace-face
  '((t :foreground "#b8c0e0"))
  "Face for TRACE."
  :group 'mini-logview)

(defface mini-logview-debug-face
  '((t :foreground "#cad3f5"))
  "Face for DEBUG."
  :group 'mini-logview)

(defface mini-logview-info-face
  '((t :foreground "#a6da95" :weight bold))
  "Face for INFO."
  :group 'mini-logview)

(defface mini-logview-warn-face
  '((t :foreground "#eed49f" :weight bold))
  "Face for WARN."
  :group 'mini-logview)

(defface mini-logview-error-face
  '((t :foreground "#ed8796" :weight bold))
  "Face for ERROR/FATAL."
  :group 'mini-logview)

(defconst mini-logview-levels
  '((1 . ("TRACE" mini-logview-trace-face))
    (2 . ("DEBUG" mini-logview-debug-face))
    (3 . ("INFO"  mini-logview-info-face))
    (4 . ("WARN"  mini-logview-warn-face))
    (5 . ("ERROR" mini-logview-error-face))
    (6 . ("FATAL" mini-logview-error-face))))

(defvar mini-logview-font-lock-keywords
  `(;; timestamp: 2026-08-18 17:32:02.598
    ("^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[ T][0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(?:\\.[0-9]+\\)?\\)"
     1 'mini-logview-timestamp-face)
    ;; thread: [main]
    ("\\[\\([^]]+\\)\\]" 1 'mini-logview-thread-face)
    ;; source file before hyphen: wm-output-manager.scm -
    ("[ \t\u00a0]+\\([a-zA-Z0-9_.-]+\\.[a-zA-Z0-9]+\\)[ \t\u00a0]+-"
     1 'mini-logview-logger-face)
    ;; pointers & scheme objects: #<pointer 0x...> or #<seat ...>
    ("#<[^>]+>" 0 'mini-logview-pointer-face)
    ;; coordinates & dimensions: 0,0 or 1445x815 or 0,345
    ("\\b[0-9]+\\(?:,[0-9]+\\|x[0-9]+\\)\\b" 0 'mini-logview-coords-face)
    ;; level keywords (matches across normal and non-breaking spaces)
    ("\\bTRACE\\b" 0 'mini-logview-trace-face)
    ("\\bDEBUG\\b" 0 'mini-logview-debug-face)
    ("\\bINFO\\b"  0 'mini-logview-info-face)
    ("\\bWARN\\b"  0 'mini-logview-warn-face)
    ("\\bERROR\\b" 0 'mini-logview-error-face)
    ("\\bFATAL\\b" 0 'mini-logview-error-face)))

(defun mini-logview-reset-filters ()
  "Show all hidden log entries."
  (interactive)
  (remove-overlays (point-min) (point-max) 'mini-logview-hidden t)
  (message "Filters cleared (showing all)"))

(defun mini-logview-filter-by-level (min-level-num)
  "Hide all log lines with severity strictly below MIN-LEVEL-NUM."
  (interactive "nMin level (1=TRACE, 2=DEBUG, 3=INFO, 4=WARN, 5=ERROR): ")
  (mini-logview-reset-filters)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-beg (line-beginning-position))
               (line-end (min (point-max) (1+ (line-end-position))))
               (line-text (buffer-substring-no-properties line-beg (line-end-position)))
               (level (cl-loop for (num name _) in mini-logview-levels
                               when (string-match-p (format "\\b%s\\b" (regexp-quote name)) line-text)
                               return num)))
          ;; Hide if it has a recognized level below threshold
          (when (and level (< level min-level-num))
            (let ((ov (make-overlay line-beg line-end)))
              (overlay-put ov 'invisible t)
              (overlay-put ov 'mini-logview-hidden t)
              (overlay-put ov 'isearch-open-invisible #'delete-overlay))
            (cl-incf count)))
        (forward-line 1)))
    (message "Applied level >= %d filter (hidden %d lines)" min-level-num count)))

(defun mini-logview--jump-to-message ()
  "Position point at the start of the message body (immediately after '- ')."
  (let ((line-end (line-end-position)))
    (beginning-of-line)
    (if (re-search-forward "[ \t\u00a0]+-[ \t\u00a0]+" line-end t)
        (goto-char (match-end 0))
      ;; Fallback: if no separator found, skip standard leading whitespace
      (back-to-indentation))))

(defun mini-logview-next-line (&optional arg)
  "Move down to next visible line and place point at message start."
  (interactive "p")
  (let ((step (or arg 1)))
    (while (> step 0)
      (forward-line 1)
      (while (and (not (eobp)) (invisible-p (point)))
        (forward-line 1))
      (setq step (1- step))))
  (unless (eobp)
    (mini-logview--jump-to-message)))

(defun mini-logview-previous-line (&optional arg)
  "Move up to previous visible line and place point at message start."
  (interactive "p")
  (let ((step (or arg 1)))
    (while (> step 0)
      (forward-line -1)
      (while (and (not (bobp)) (invisible-p (point)))
        (forward-line -1))
      (setq step (1- step))))
  (unless (eobp)
    (mini-logview--jump-to-message)))

(defvar mini-logview-mode-map
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (define-key map (kbd "n") #'mini-logview-next-line)
    (define-key map (kbd "p") #'mini-logview-previous-line)
    (define-key map (kbd "j") #'mini-logview-next-line)
    (define-key map (kbd "k") #'mini-logview-previous-line)

    ;; level filters
    (define-key map (kbd "1") (lambda () (interactive) (mini-logview-filter-by-level 1)))
    (define-key map (kbd "2") (lambda () (interactive) (mini-logview-filter-by-level 2)))
    (define-key map (kbd "3") (lambda () (interactive) (mini-logview-filter-by-level 3)))
    (define-key map (kbd "4") (lambda () (interactive) (mini-logview-filter-by-level 4)))
    (define-key map (kbd "5") (lambda () (interactive) (mini-logview-filter-by-level 5)))
    (define-key map (kbd "0") #'mini-logview-reset-filters)
    (define-key map (kbd "a") #'mini-logview-reset-filters)
    map)
  "Keymap for `mini-logview-mode`.")

;;;###autoload
(define-derived-mode mini-logview-mode special-mode "Mini-Logview"
  "Major mode for viewing structured logs."
  :group 'mini-logview
  (setq-local font-lock-defaults '(mini-logview-font-lock-keywords t nil nil nil))
  (setq-local truncate-lines t)
  (font-lock-flush))

(provide 'mini-logview)
;;; mini-logview.el ends here
