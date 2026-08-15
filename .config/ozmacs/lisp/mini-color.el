;;; mini-color-mode.el --- Minimal color code highlighter -*- lexical-binding: t; -*-

(require 'color)

(defgroup mini-color nil
  "Highlight color codes in buffer."
  :group 'font-lock
  :prefix "mini-color-")

(defconst mini-color--hex-regex
  "#\\(?:[0-9a-fA-F]\\{3\\}\\|[0-9a-fA-F]\\{6\\}\\)\\b"
  "Regexp matching 3-digit and 6-digit hex colors.")

(defun mini-color--contrast-color (hex-or-name)
  "Return \"#000000\" or \"#ffffff\" to ensure high contrast against HEX-OR-NAME."
  (condition-case nil
      (let* ((rgb (color-name-to-rgb hex-or-name))
             ;; relative luminance calculation
             (luminance (+ (* 0.299 (nth 0 rgb))
                           (* 0.587 (nth 1 rgb))
                           (* 0.114 (nth 2 rgb)))))
        (if (> luminance 0.55) "#000000" "#ffffff"))
    (error "#000000")))

(defun mini-color--matcher (limit)
  "Match color strings and apply dynamically computed background faces up to LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward mini-color--hex-regex limit t))
      (let* ((color-str (match-string-no-properties 0))
             (fg (mini-color--contrast-color color-str)))
        (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face
         `(:background ,color-str :foreground ,fg))
        (setq found t)))
    found))

(defvar mini-color--font-lock-keywords
  '((mini-color--matcher)))

;;;###autoload
(define-minor-mode mini-color-mode
  "Highlight color codes in the current buffer with their actual color."
  :lighter " Color"
  (if mini-color-mode
      (font-lock-add-keywords nil mini-color--font-lock-keywords 'append)
    (font-lock-remove-keywords nil mini-color--font-lock-keywords)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(face nil))))
  (when font-lock-mode
    (font-lock-flush)))

;;;###autoload
(define-globalized-minor-mode global-mini-color-mode
  mini-color-mode
  (lambda () (when (derived-mode-p 'prog-mode 'css-mode 'web-mode 'text-mode)
               (mini-color-mode 1))))

(provide 'mini-color-mode)
;;; mini-color-mode.el ends here
