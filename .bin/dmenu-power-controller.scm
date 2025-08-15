#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 format)
			 (srfi srfi-9)
             (srfi srfi-13)
			 (srfi srfi-18)
			 (srfi srfi-26)
			 (srfi srfi-1))

(include "dmenu-base.scm")

(define options
  '("lock"
    "logout"
    "suspend"
    "scheduled suspend (10min)"
    "scheduled suspend (20min)"
    "scheduled suspend (30min)"
    "reboot"
    "shutdown"))

(let* ((choice (dmenu-prompt "Power Menu > "
			    options)))
  (cond
   ((string=? choice (list-ref options 0)) (run-command "swaylock"))
   ((string=? choice (list-ref options 1)) (run-command "swaymsg exit"))
   ((string=? choice (list-ref options 2)) (run-command "systemctl suspend"))
   ((string=? choice (list-ref options 3)) (run-command "sleep 600 && systemctl suspend"))
   ((string=? choice (list-ref options 4)) (run-command "sleep 1200 && systemctl suspend"))
   ((string=? choice (list-ref options 5)) (run-command "sleep 1800 && systemctl suspend"))
   ((string=? choice (list-ref options 6)) (run-command "reboot"))
   ((string=? choice (list-ref options 7)) (run-command "shutdown"))
   (else #f)))
