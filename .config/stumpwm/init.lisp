;; #-loading quicklisp
(in-package :stumpwm)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (prog
      (load quicklisp-init)
      ;; setting font, needs a stumpwm module and quicklisp package
      (ql:quickload :clx-truetype))))

;; (set-module-dir "/usr/share/stumpwm/contrib/")
(init-load-path "~/.config/stumpwm/modules")
(init-load-path "~/.guix-home/profile/share/common-lisp/sbcl")

;; load required modules if they exist
(loop for module in '("globalwindows" "ttf-fonts" "swm-gaps" "swm-external-panel")
  do (when (find-module module)
    (load-module module)))


(which-key-mode)

;; behavior
(setf *mouse-focus-policy* :click)
(setf *run-or-raise-all-groups* nil)
