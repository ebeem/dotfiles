;;;; swm-calibre.asd

(asdf:defsystem #:swm-external-panel
  :description "Integrates unmanaged external panels with StumpWM"
  :author "Almarhoon Ibraheem <https://github.com/ebeem>"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "swm-external-panel")))
