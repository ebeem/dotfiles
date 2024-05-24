(define-module (hypr utility)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (json)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (sublist
            fix-arguments-length
            fix-arguments-type
            get-command-output
            dispatch-command
            hypr-command
            write-to-socket))

(define (sublist l offset n)
  (take (drop l offset) n))

(define (fix-arguments-length lst target-length)
  (cond
   ((eq? target-length 0) '())
   ((< (length lst) target-length)
    (append lst (make-list (- target-length (length lst)) #f)))
   ((> (length lst) target-length)
    (append
     (sublist lst 0 (- target-length 1))
     (list (string-join (sublist lst target-length (- (length lst) target-length)) ","))))
    (else lst)))

(define (get-command-output command)
  (let* ((port (open-input-pipe command))
       (str  (read-string port)))
  (close-pipe port)
  str))

(define (dispatch-command command)
  (display (string-append "hyprctl dispatch " command))
  (newline)
  (get-command-output (string-append "hyprctl dispatch " command)))

(define (hypr-command command)
  (get-command-output (string-append "hyprctl -j " command)))

(define (write-to-socket socket-path data)
  (call-with-output-file socket-path
    (lambda (port)
      (display data port))))
