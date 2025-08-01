;;; oz-evil.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:
(use-package meow
  :ensure t
  :config
  (setq meow-mode-state-list
        '((conf-mode . normal)
          (fundamental-mode . normal)
          (help-mode . normal)
          (prog-mode . normal)
          (man-mode . normal)
          (help-mode . normal)
          (text-mode . normal)
          (messages-buffer-mode . normal)
          (gnus-group-mode . motion)
          (elfeed-view-mode . normal)
          (elfeed-show-mode . normal)))

  (setq meow-keypad-leader-dispatch "C-c")
  ;; (setq meow-use-keypad-when-execute-kbd nil)
  (setq meow-two-char-escape-sequence "jk")
  (setq meow-two-char-escape-delay 0.5)

  (defun meow--two-char-exit-insert-state (s)
    (when (meow-insert-mode-p)
      (let ((modified (buffer-modified-p)))
        (insert (elt s 0))
        (let* ((second-char (elt s 1))
               (event
                (if defining-kbd-macro
                    (read-event nil nil)
                  (read-event nil nil meow-two-char-escape-delay))))
          (when event
            (if (and (characterp event) (= event second-char))
                (progn
                  (backward-delete-char 1)
                  (set-buffer-modified-p modified)
                  (meow--execute-kbd-macro "<escape>"))
              (push event unread-command-events)))))))

  (defun meow-two-char-exit-insert-state ()
    (interactive)
    (meow--two-char-exit-insert-state meow-two-char-escape-sequence))

  (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
              #'meow-two-char-exit-insert-state)

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right)
   '("{" . "M-{")
   '("}" . "M-}")
   '("<escape>" . ignore))

  (defun goto-match-paren ()
    "Go to the matching parenthesis if on a parenthesis character."
    (interactive)
    (cond ((looking-at "\\s(") (forward-sexp 1))
          ((looking-back "\\s)" 1) (backward-sexp 1))
          (t (message "Not on a parenthesis"))))
  
(defun delete-region-and-yank ()
  "Delete the selected region (if any), then yank from the clipboard."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (yank))

(defun eb/show-custom-mode-keymap ()
  "Pretend user pressed C-c"
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "C-c"))))

(meow-normal-define-key
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("0" . beginning-of-line)
   '("$" . end-of-line)
   '("%" . goto-match-paren)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("{" . "M-{")
   '("}" . "M-}")
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . isearch-repeat-forward)
   '("N" . isearch-repeat-backward)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . delete-region-and-yank)
   '("P" . clipboard-yank)
   '("Q" . meow-goto-line)
   '("r" . undo-redo)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("S" . meow-clipboard-kill)
   '("t" . meow-till)
   '("u" . undo-only)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("y" . meow-clipboard-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . isearch-forward)
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)
   '(":" . meow-goto-line)
   '("SPC" . eb/show-custom-mode-keymap)
   '("<escape>" . ignore))

  (add-hook 'elfeed-show-mode-hook #'my/elfeed-meow-keys)
  (add-hook 'mu4e-view-mode-hook #'my/mu4e-view-meow-keys)
  
  (meow-global-mode 1))

(provide 'oz-meow)
;;; oz-evil.el ends here
