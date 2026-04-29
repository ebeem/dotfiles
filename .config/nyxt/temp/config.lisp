(defvar one-dark (make-instance 'theme:theme
   :background-color "#282c34"
   :on-background-color "#bbc2cf"
   :background-alt-color   "#21242b"
   :on-background-alt-color "5B6268"

   :primary-color          "#51afef"
   :on-primary-color       "black"
   :primary-alt-color      "#686868"
   :on-primary-alt-color   "white"
   :secondary-color        "#7042a2"
   :on-secondary-color     "white"
   :secondary-alt-color    "#909090"
   :on-secondary-alt-color "black"

   :accent-color           "#51afef"
   :on-accent-color        "#bbc2cf"
   :accent-alt-color       "#178DCC"
   :on-accent-alt-color    "black"
   :warning-color          "#AF1923"
   :on-warning-color       "white"
   :warning-alt-color      "#D2232E"
   :on-warning-alt-color   "white"
   :font-family "Iosevka"))

;; Set the theme in Nyxt's config file
(define-configuration browser ((theme one-dark)))

;; (define-nyxt-user-system-and-load "nyxt-user/dark-reader"
;;   ;; Remove this line if you don't need the file.
;;   :components ("nx-dark-reader/nx-dark-reader.lisp")
;;   :depends-on (:nx-dark-reader))
