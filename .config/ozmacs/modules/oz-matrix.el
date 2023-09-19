

;;; Code:
(use-package plz
  :elpaca (:host github :repo "alphapapa/plz.el"))

(use-package ement
  :after plz
  :commands (ement-connect)
  :elpaca (:host github :repo "alphapapa/ement.el")
  :hook (ement-room-view . (lambda (room session) (ement-view-mode-enhanced)))
  :init
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
