;; #-loading quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(set-module-dir "/usr/share/stumpwm/contrib/")
(which-key-mode)

;;; Emacs integration
(defcommand emacs () () ; override default emacs command
  "Start emacs if emacsclient is not running and focus emacs if it is
running in the current group"
  (run-or-raise "emacsclient -c -a 'emacs'" '(:class "Emacs")))

(load-module "swm-emacs")
;; Treat emacs splits like Xorg windows
(defun eval-string-as-el (elisp &optional collect-output-p)
  "evaluate a string as emacs lisp"
  (let ((result (run-shell-command
                 (format nil "timeout --signal=9 1m emacsclient --eval \"~a\""
                         elisp)
                 collect-output-p)))
    (message result)
    (handler-case (read-from-string result)
      ;; Pass back a string when we can't read from the string
      (error () result))))

(defun is-emacs-p (win)
  "nil if the WIN"
  (when win
    (string-equal (window-class win) "Emacs")))

(defun emacs-winmove (direction)
  "executes the emacs function winmove-DIRECTION where DIRECTION is a string"
  (eval-string-as-el (concat "(windmove-" direction ")") t))

;;; Window focusing
(defcommand improved-move-focus (dir) ((:direction "Direction: "))
  "Similar to move-focus but also treats emacs windows as Xorg windows"
  (declare (type (member :up :down :left :right) dir))
  (if (is-emacs-p (current-window))
    (when ;; There is not emacs window in that direction
      (length= (emacs-winmove (string-downcase (string-trim ":" (string dir)))) 1)
        (move-focus dir))
    (move-focus dir)))

;; behavior
(setf *mouse-focus-policy* :click)
(setf *run-or-raise-all-groups* nil)
