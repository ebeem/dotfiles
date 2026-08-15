;;; mini-rainbow-delimiters.el --- Minimal rainbow delimiters -*- lexical-binding: t; -*-

(require 'jit-lock)

(defgroup mini-rainbow-delimiters nil
  "Highlight delimiters according to their nesting depth."
  :group 'faces
  :prefix "mini-rainbow-delimiters-")

(defface mini-rainbow-delimiters-unmatched-face '((t :inherit error :weight bold))          "Unmatched delimiter face." :group 'mini-rainbow-delimiters)
(eval-when-compile
  (defmacro mini-rainbow-delimiters--define-depth-faces ()
    (let ((faces '())
          (light-colors ["#707183" "#7388d6" "#909183" "#709870" "#907373"
                         "#6276ba" "#858580" "#80a880" "#887070"])
          (dark-colors ["grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
                        "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"]))
      (dotimes (i 9)
        (push `(defface ,(intern (format "mini-rainbow-delimiters-depth-%d-face" (1+ i)))
                 '((default (:inherit default))
                   (((class color) (background light)) :foreground ,(aref light-colors i))
                   (((class color) (background dark)) :foreground ,(aref dark-colors i)))
                 ,(format "Nested delimiter face, depth %d." (1+ i))
                 :group 'mini-rainbow-delimiters-faces)
              faces))
      `(progn ,@faces))))
(mini-rainbow-delimiters--define-depth-faces)

(defvar mini-rainbow-delimiters-faces
  [mini-rainbow-delimiters-depth-1-face
   mini-rainbow-delimiters-depth-2-face
   mini-rainbow-delimiters-depth-3-face
   mini-rainbow-delimiters-depth-4-face
   mini-rainbow-delimiters-depth-5-face
   mini-rainbow-delimiters-depth-6-face
   mini-rainbow-delimiters-depth-7-face
   mini-rainbow-delimiters-depth-8-face
   mini-rainbow-delimiters-depth-9-face]
  "Vector of depth faces to cycle through.")

(defun mini-rainbow-delimiters--propertize (start end)
  "Colorize delimiters between START and END using incremental syntax parsing."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (let ((prev start)
            (state (syntax-ppss start)))
        (while (re-search-forward "[][(){}]" end t)
          (let* ((pos (match-beginning 0))
                 (char (char-after pos)))
            ;; Advance parse state through the delimiter character
            (setq state (parse-partial-sexp prev (1+ pos) nil nil state))
            (setq prev (1+ pos))
            ;; Skip delimiters inside strings (nth 3) or comments (nth 4)
            (unless (or (nth 3 state) (nth 4 state))
              (let* ((is-open (memq char '(?\( ?\[ ?\{)))
                     ;; For opening delims, use new depth; for closing delims, matching depth was pre-close
                     (depth (if is-open (nth 0 state) (1+ (nth 0 state))))
                     (face (if (> depth 0)
                               (aref mini-rainbow-delimiters-faces
                                     (mod (1- depth) (length mini-rainbow-delimiters-faces)))
                             'mini-rainbow-delimiters-unmatched-face)))
                (put-text-property pos (1+ pos) 'face face)))))))))

;;;###autoload
(define-minor-mode mini-rainbow-delimiters-mode
  "Highlight delimiters according to their depth."
  :lighter ""
  (if mini-rainbow-delimiters-mode
      (jit-lock-register #'mini-rainbow-delimiters--propertize t)
    (jit-lock-unregister #'mini-rainbow-delimiters--propertize)
    (with-silent-modifications
      (font-lock-flush))))

;;;###autoload
(define-globalized-minor-mode global-mini-rainbow-delimiters-mode
  mini-rainbow-delimiters-mode
  (lambda () (when (derived-mode-p 'prog-mode)
               (mini-rainbow-delimiters-mode 1))))

(provide 'mini-rainbow-delimiters)
;;; mini-rainbow-delimiters.el ends here
