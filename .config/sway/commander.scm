#!/usr/bin/guile
!#
;; #!/usr/bin/guile --fresh-auto-compile
;; ensure that the swayipc module is available under the same directory as the init file
;; otherwise, the module should be referenced from packaging system or via custom load path

(add-to-load-path
 (dirname (if (current-filename)
              (current-filename)
              (string-append (getenv "HOME") "/.config/sway/init.scm"))))

(use-modules (swayipc connection)
             (ice-9 popen)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (rnrs io ports)
             (oop goops)
             (srfi srfi-18)
             (srfi srfi-9)
             (srfi srfi-1))

(define COMMANDS-CLIENT-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(display (string-append "connecting to " SOCKET-COMMANDS-LISTENER-PATH "\n"))
(connect COMMANDS-CLIENT-SOCKET
         (make-socket-address AF_UNIX SOCKET-COMMANDS-LISTENER-PATH))
(display "connected\n")

(define (send-command command)
  (display (string-append "sending command: " command "\n"))
  (display (write-msg COMMANDS-CLIENT-SOCKET
             RUN-COMMMAND-MSG-ID
             command))
  (display "sent\n"))

(send-command "test")
