;;; mini-hl-todo.el --- Minimal TODO keyword highlighting -*- lexical-binding: t; -*-

(defgroup mini-hl-todo nil
  "Highlight TODO and similar keywords in comments."
  :group 'font-lock
  :prefix "mini-hl-todo-")

(defcustom mini-hl-todo-keyword-faces
  '(("TODO"       . (:foreground "#ECBE7B" :weight bold))
    ("FIXME"      . (:foreground "#ff6c6b" :weight bold))
    ("REVIEW"     . (:foreground "#46D9FF" :weight bold))
    ("HACK"       . (:foreground "#ECBE7B" :weight bold))
    ("DEPRECATED" . (:foreground "#c678dd" :weight bold))
    ("NOTE"       . (:foreground "#98be65" :weight bold))
    ("BUG"        . (:foreground "#ff6c6b" :weight bold)))
  "Alist mapping keywords to their display face attributes."
  :type '(alist :key-type string :value-type plist)
  :group 'mini-hl-todo)

(defvar-local mini-hl-todo--regexp nil
  "Buffer-local compiled regexp for active keywords.")

(defun mini-hl-todo--get-face ()
  "Return the face plist for the currently matched keyword."
  (cdr (assoc (match-string-no-properties 0) mini-hl-todo-keyword-faces)))

(defun mini-hl-todo--matcher (limit)
  "Search for keywords up to LIMIT, restricting matches to comments."
  (let (found)
    (while (and (not found)
                mini-hl-todo--regexp
                (re-search-forward mini-hl-todo--regexp limit t))
      (let ((syntax (syntax-ppss)))
        ;; Only highlight if point is inside a comment (or in text-mode)
        (when (or (nth 4 syntax)
                  (derived-mode-p 'text-mode))
          (setq found t))))
    found))

(defconst mini-hl-todo--font-lock-keywords
  '((mini-hl-todo--matcher (0 (mini-hl-todo--get-face) prepend t)))
  "Font-lock keywords for `mini-hl-todo-mode`.")

;;;###autoload
(define-minor-mode mini-hl-todo-mode
  "Toggle highlighting of TODO keywords in the current buffer."
  :lighter " hl-todo"
  (if mini-hl-todo-mode
      (progn
        (setq-local mini-hl-todo--regexp
                    (concat "\\<"
                            (regexp-opt (mapcar #'car mini-hl-todo-keyword-faces))
                            "\\>"))
        (font-lock-add-keywords nil mini-hl-todo--font-lock-keywords 'append))
    (font-lock-remove-keywords nil mini-hl-todo--font-lock-keywords)
    (kill-local-variable 'mini-hl-todo--regexp))
  (when font-lock-mode
    (font-lock-flush)))

;;;###autoload
(define-globalized-minor-mode global-mini-hl-todo-mode
  mini-hl-todo-mode
  (lambda () (when (derived-mode-p 'prog-mode 'text-mode)
               (mini-hl-todo-mode 1))))

(provide 'mini-hl-todo)
;;; mini-hl-todo.el ends here
