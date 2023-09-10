

;;; Code:
(use-package plz
  :elpaca (:host github :repo "alphapapa/plz.el"))

(use-package ement
  :after plz
  :commands (ement-connect)
  :elpaca (:host github :repo "alphapapa/ement.el")
  :config
  (setq ement-save-sessions t))

(provide 'oz-matrix)
;;; oz-matrix.el ends here
