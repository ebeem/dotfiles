(define-module (swayipc connection)
  ;; #:use-module (swayipc utility)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (SOCKET-PATH
            COMMAND-SOCKET
            MSG-MAGIC
            MSG-MAGIC-BV

            write-msg
            read-msg
            encode-msg
            bytevector-concatenate))

(define MSG-MAGIC "i3-ipc")
(define MSG-MAGIC-BV (string->utf8 MSG-MAGIC))
;; TODO: maybe also get from sway and i3 binaries
(define SOCKET-PATH
  (and (getenv "SWAYSOCK")
       (getenv "I3SOCK")))

(define COMMAND-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(connect COMMAND-SOCKET (make-socket-address AF_UNIX SOCKET-PATH))

(define (bytevector-concatenate . bvs)
  (let* ((len (apply + (map (lambda (bv) (bytevector-length bv)) bvs)))
         (result (make-bytevector len))
         (index 0))
    (for-each
     (lambda (bv)
       (bytevector-copy! bv 0 result index (bytevector-length bv))
       (set! index (+ index (bytevector-length bv))))
     bvs)
    result))

;; The format for messages and replies is:
;;     <magic-string> <payload-length> <payload-type> <payload>
;; Where
;;      <magic-string> is i3-ipc, for compatibility with i3
;;      <payload-length> is a 32-bit integer in native byte order
;;      <payload-type> is a 32-bit integer in native byte order
(define (encode-msg command-id payload)
  (let* ((bv (make-bytevector (+ 14 (string-length payload)))))
    ;; <magic-string> <payload-length> <payload-type> <payload>
    (bytevector-copy! (string->utf8 "i3-ipc") 0 bv 0 6)
    (bytevector-u32-set! bv 6 (string-length payload) (endianness little))
    (bytevector-u32-set! bv 10 command-id (endianness little))

    ;; payload is optional
    (when (> (string-length payload) 0)
      (bytevector-copy! (string->utf8 payload) 0 bv 14 (string-length payload)))
    bv))

(define (write-msg sock command-id payload)
  (put-bytevector sock (encode-msg command-id payload)))

(define (read-msg sock)
  (let* ((bv-header (get-bytevector-n sock 14))
         (payload-length (bytevector-u32-ref bv-header 6 (endianness little)))
         (command-id (bytevector-u32-ref bv-header 10 (endianness little)))
         (payload (utf8->string (get-bytevector-n sock payload-length))))
    command-id payload))

;; (define (read-from-socket socket-path)
;;   (let* ((sock (socket AF_UNIX SOCK_STREAM 0))
;;          (socket-addr (make-socket-address AF_UNIX socket-path))
;;          (bv (make-bytevector 14)))

;;     (connect sock socket-addr)
;;     (let loop ()
;;       (let ((data (read-line sock)))
;;         (if (eof-object? data)
;;             (begin
;;               (close-port sock)
;;               (display "Connection closed\n"))
;;             (begin
;;               (display data)
;;               (let* ((command-id (substring data 0 (string-index data #\>)))
;;                      (params (string-split (substring data (+ 2 (string-index data #\>))) #\,)))
;;                 (handle-event command-id params)
;;                 (loop))))))))

;; (define (start-event-listener)
;;   (read-from-socket SOCKET-PATH))

;; (define listener-thread (make-thread start-event-listener))
;; (thread-start! listener-thread)
;; (thread-join! listener-thread)
