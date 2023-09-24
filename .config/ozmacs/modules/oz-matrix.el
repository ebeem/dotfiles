

;;; Code:
(use-package plz
  :elpaca (:host github :repo "alphapapa/plz.el"))

(use-package ement
  :after plz
  :commands (ement-connect)
  :elpaca (:host github :repo "alphapapa/ement.el")
  :hook (ement-room-view . (lambda (room session) (ement-view-mode-enhanced)))
  :init
  (with-eval-after-load 'savehist
      (defun ement--savehist-save-hook ()
        "Remove all `ement-' commands from `command-history'.
    Because when `savehist' saves `command-history', it includes the
    interactive arguments passed to the command, which in our case
    includes large data structures that should never be persisted!"
        (setf command-history
              (cl-remove-if (pcase-lambda (`(,command . ,_))
                              (string-match-p (rx bos "ement-") (symbol-name command)))
                            command-history)))
      (add-hook 'begining-of-buffer-hook (lambda () (ement-room-mwheel-scroll)))
      (cl-pushnew 'ement--savehist-save-hook savehist-save-hook))
  (setq ement-save-sessions t)
  :config
  (defun ement-view-mode-enhanced ()
    (setq-local visual-fill-column-width 150
                visual-fill-column-center-text t)
    ;; (variable-pitch-mode t)
    (visual-fill-column-mode 1))
  (setq ement-room-message-format-spec "%S%L:\s%B%r%R%t"
        ement-room-left-margin-width 10
        ement-room-right-margin-width 0))

(provide 'oz-matrix)
;;; oz-matrix.el ends here
