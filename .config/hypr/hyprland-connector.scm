; Importing necessary libraries
(import (chicken sockets) (chicken process))

; Define the command to send
(define (command-send cmd)
  (let ((sock (socket AF_UNIX SOCK_STREAM)))
    (socket-connect! sock (string-append "/tmp/hypr/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/.socket.sock"))
    (socket-send! sock (string->utf8 cmd))
    (let ((resp (socket-recv! sock 100)))
      (if (not (equal? resp (string->utf8 "ok")))
          (error (string-append "Hyprland Error: " cmd " : " resp))))
    (socket-close sock)))

; Define the async command send
(define (async-command-send cmd ignore-ok)
  (let* ((connection (open-connection (string-append "/tmp/hypr/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/.socket.sock")))
         (reader (car connection))
         (writer (cdr connection)))
    (socket-send! writer (string->utf8 cmd))
    (sync (flush-output writer))
    (let ((resp (socket-recv! reader 100)))
      (socket-close writer)
      (if (and (not ignore-ok) (not (equal? resp (string->utf8 "ok"))))
          (error (string-append "hyprland: " cmd " : " resp))
          resp))))

; Define the async hyprctl
(define (async-hyprctl cmd)
  (let* ((p (pipe))
         (process (make-process OPEN_WRITE (string-append "hyprctl " cmd) p)))
    (process-wait process)
    (let ((resp (read-string (pipe-read p))))
      (string-trim resp))))

; Define the keyword class
(define-class keyword ()
  (define-public (send-cmd attr value)
    (if (boolean? value)
        (set! value (string-downcase (->string value))))
    (async-command-send (string-append "keyword " (string-downcase (type-name this)) ":" (string-replace "__" "." attr) " " (->string value)) #f))
  
  (define (set! attr value)
    (if (not ignore)
        (if (boolean? value)
            (set! value (string-downcase (->string value))))
        (command-send (string-append "keyword " (string-downcase (type-name this)) ":" (string-replace "__" "." attr) " " value)))
    (super-set! attr value)))

; Define the BindListener class
(define-class BindListener ()
  (define-public (send-bind bind)
    (let ((cmd (if (string? (get-field bind 'f))
                   (string-append "keyword bind" (get-field bind 'flag) " \"" (string-join (get-field bind 'key) ",") "," (get-field bind 'f) "\"")
                   (string-append "keyword bind" (get-field bind 'flag) " \"" (string-join (get-field bind 'key) ",") ",exec,echo bind." (string-join (get-field bind 'key) ".") " | socat unix-connect:/tmp/hypr_py/$HYPRLAND_INSTANCE_SIGNATURE/.socket.sock STDIO\""))))
      (async-hyprctl cmd)))
  
  (define-public (handle-bind reader writer)
    (let ((data (read-string (socket-recv! reader 1024))))
      (if (string-prefix? data "bind.")
          (let ((data (string-trim (string-drop data 5))))
            (for-each (lambda (bind)
                        (if (equal? (string-join (get-field bind 'key) ".") data)
                            (begin
                              (apply (get-field bind 'f) (get-field bind 'args))
                              (socket-send! writer "ok")
                              (sync (flush-output writer))
                              (return (socket-close writer)))))
                      (get-field this 'config '_binds)))
          (begin
            (socket-send! writer "not ok")
            (sync (flush-output writer))
            (socket-close writer)))))
  
  (define-public (start)
    (if (not (file-exists? "/tmp/hypr_py/"))
        (make-directory "/tmp/hypr_py/"))
    (if (not (file-exists? (string-append "/tmp/hypr_py/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/")))
        (make-directory (string-append "/tmp/hypr_py/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/")))
    (if (file-exists? (string-append "/tmp/hypr_py/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/.socket.sock"))
        (delete-file (string-append "/tmp/hypr_py/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/.socket.sock")))
    (let ((server (socket-listen (string-append "/tmp/hypr_py/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/.socket.sock"))))
      (while #t
        (let ((connection (accept-connection server)))
          (let ((reader (car connection))
                (writer (cdr connection)))
            (handle-bind reader writer)))))))

; Define the EventListener class
(define-class EventListener ()
  (define-public (start)
    (let ((sock (socket AF_UNIX SOCK_STREAM)))
      (socket-connect! sock (string-append "/tmp/hypr/" (getenv "HYPRLAND_INSTANCE_SIGNATURE") "/.socket2.sock"))
      (yield "connect")
      (let loop ()
        (let ((data (socket-recv! sock 1024)))
          (if (not (equal? data ""))
              (begin
                (yield (utf8->string data))
                (loop))))))))
