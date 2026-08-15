(require 'dired)

(defgroup mini-diredfl nil
  "Minimal font-lock enhancements for Dired."
  :group 'dired
  :prefix "mini-diredfl-")

;; Faces
(defface mini-diredfl-dir-priv
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for directory 'd' bit."
  :group 'mini-diredfl)

(defface mini-diredfl-read-priv
  '((t :inherit default))
  "Face for read 'r' permissions."
  :group 'mini-diredfl)

(defface mini-diredfl-write-priv
  '((t :inherit warning))
  "Face for write 'w' permissions."
  :group 'mini-diredfl)

(defface mini-diredfl-exec-priv
  '((t :inherit error :weight bold))
  "Face for exec 'x'/'s' permissions."
  :group 'mini-diredfl)

(defface mini-diredfl-no-priv
  '((t :inherit shadow))
  "Face for '-' permission slots."
  :group 'mini-diredfl)

(defface mini-diredfl-size
  '((t :inherit shadow))
  "Face for file sizes."
  :group 'mini-diredfl)

(defface mini-diredfl-date
  '((t :inherit font-lock-string-face))
  "Face for dates and timestamps."
  :group 'mini-diredfl)

(defvar mini-diredfl--font-lock-keywords
  `(;; Permissions: match 10-char file mode string (e.g. drwxr-xr-x)
    ("^ *\\([bcdlps-][r-][w-][x-][r-][w-][x-][r-][w-][x-]\\)"
     (1 (let ((str (match-string 1)))
          (dotimes (i (length str))
            (let ((c (aref str i))
                  (pos (+ (match-beginning 1) i)))
              (put-text-property
               pos (1+ pos) 'face
               (pcase c
                 (?d 'mini-diredfl-dir-priv)
                 (?r 'mini-diredfl-read-priv)
                 (?w 'mini-diredfl-write-priv)
                 ((or ?x ?s ?t) 'mini-diredfl-exec-priv)
                 (?- 'mini-diredfl-no-priv)
                 (_  'default)))))
          nil)))
    ;; File size (numeric or human-readable like 4.2K, 12M, 1.1G)
    (" [0-9]+\\(?:\\.[0-9]+\\)?[BkKMGTPEZY]? "
     0 'mini-diredfl-size prepend)
    ;; Standard timestamps: Jan 12 10:45 or 2026-08-14 19:20
    ("\\(?:[A-Z][a-z]\\{2\\} +[0-9]\\{1,2\\} +\\(?:[0-9]\\{4\\}\\|[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\|[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +[0-9]\\{2\\}:[0-9]\\{2\\}\\)"
     0 'mini-diredfl-date prepend))
  "Font-lock keywords for `mini-diredfl-mode`.")

;;;###autoload
(define-minor-mode mini-diredfl-mode
  "Minor mode for lightweight Dired font-locking."
  :lighter ""
  (if mini-diredfl-mode
      (font-lock-add-keywords nil mini-diredfl--font-lock-keywords 'append)
    (font-lock-remove-keywords nil mini-diredfl--font-lock-keywords))
  (when font-lock-mode
    (font-lock-flush)))

;;;###autoload
(define-globalized-minor-mode global-mini-diredfl-mode
  mini-diredfl-mode
  (lambda () (when (derived-mode-p 'dired-mode) (mini-diredfl-mode 1))))

(provide 'mini-diredfl)
