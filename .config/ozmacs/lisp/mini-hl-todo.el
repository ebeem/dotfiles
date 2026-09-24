;;; mini-hl-todo.el --- Minimal TODO keyword highlighting -*- lexical-binding: t; -*-

(defgroup mini-hl-todo nil
  "Highlight TODO and similar keywords in comments and strings."
  :group 'font-lock
  :prefix "mini-hl-todo-")

(defface mini-hl-todo
  '((t (:weight bold)))
  "Base face used to highlight keywords."
  :group 'mini-hl-todo)

;; highlight TODO:FIXME:REVIEW:HACK:DEPRECATED:NOTE:BUG
(defcustom mini-hl-todo-keyword-faces
  '(("TODO"       . "#ECBE7B")
    ("FIXME"      . "#ff6c6b")
    ("REVIEW"     . "#46D9FF")
    ("HACK"       . "#ECBE7B")
    ("DEPRECATED" . "#c678dd")
    ("NOTE"       . "#98be65")
    ("BUG"        . "#ff6c6b"))
  "Alist mapping keywords to hex colors, face symbols, or plists."
  :type '(alist :key-type string :value-type (choice string face plist))
  :group 'mini-hl-todo)

(defvar mini-hl-todo-syntax-table (copy-syntax-table text-mode-syntax-table)
  "Syntax table used while searching for keywords.")

(defvar-local mini-hl-todo--regexp nil
  "Buffer-local compiled regexp for active keywords.")

(defun mini-hl-todo--regexp ()
  "Return or compile the keyword regular expression."
  (or mini-hl-todo--regexp
      (setq mini-hl-todo--regexp
            (concat "\\(\\_<\\("
                    (mapconcat (lambda (k) (regexp-quote (car k)))
                               mini-hl-todo-keyword-faces
                               "\\|")
                    "\\)\\_>\\)"))))

(defun mini-hl-todo--combine-face (face-or-color)
  "Combine FACE-OR-COLOR with the `mini-hl-todo' base face for font-lock."
  (cond
   ((stringp face-or-color)
    `((:foreground ,face-or-color) mini-hl-todo))
   ((listp face-or-color)
    `(,face-or-color mini-hl-todo))
   (t face-or-color)))

(defun mini-hl-todo--get-face ()
  "Return the combined face for the matched keyword (group 2)."
  (mini-hl-todo--combine-face
   (cdr (assoc (match-string-no-properties 2) mini-hl-todo-keyword-faces))))

(defun mini-hl-todo--matcher (bound)
  "Search for keywords up to BOUND inside comments, strings, or text-mode."
  (catch 'found
    (while (let ((case-fold-search nil)
                 (syntax-ppss-table (syntax-table)))
             (with-syntax-table mini-hl-todo-syntax-table
               (re-search-forward (mini-hl-todo--regexp) bound t)))
      (cond ((or (derived-mode-p 'text-mode)
                 (nth 8 (syntax-ppss)))
             (throw 'found t))
            ((and bound (>= (point) bound))
             (throw 'found nil))))))

(defconst mini-hl-todo--keywords
  `((mini-hl-todo--matcher
     (1 (mini-hl-todo--get-face) prepend t)))
  "Font-lock specification for `mini-hl-todo-mode`.")

;;;###autoload
(define-minor-mode mini-hl-todo-mode
  "Toggle highlighting of TODO keywords in the current buffer."
  :lighter " hl-todo"
  (if mini-hl-todo-mode
      (font-lock-add-keywords nil mini-hl-todo--keywords t)
    (font-lock-remove-keywords nil mini-hl-todo--keywords)
    (kill-local-variable 'mini-hl-todo--regexp))
  (when font-lock-mode
    (font-lock-flush)))

(defun mini-hl-todo--turn-on ()
  "Enable `mini-hl-todo-mode` in supported programming and text modes."
  (when (and (derived-mode-p 'prog-mode 'text-mode)
             (not (derived-mode-p 'org-mode)))
    (mini-hl-todo-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-mini-hl-todo-mode
  mini-hl-todo-mode
  mini-hl-todo--turn-on)

(provide 'mini-hl-todo)
;;; mini-hl-todo.el ends here

