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

(define (assq-delete-all k al)
  (cond [(null? al) '()]
        [(eq? (caar al) k) (assq-delete-all k (cdr al))]
        [else (cons (car al) (assq-delete-all k (cdr al)))]))

(define (alist-set al k v)
  (cons (cons k v) (assq-delete-all k al)))

(define (parse-ini path)
  (define (strip-inline-comment s)
    (let* ((semi (string-index s #\;))
           (hash (string-index s #\#))
           (cut (cond [(and semi hash) (min semi hash)]
                      [semi semi]
                      [hash hash]
                      [else #f])))
      (if cut (substring s 0 cut) s)))
  (call-with-input-file path
    (lambda (port)
      (let loop ((sect #f) (tbl '()))
        (let ((raw (read-line port 'concat)))
          (if (eof-object? raw)
              tbl
              (let* ((line (string-trim-both raw)))
                (cond
                 [(or (zero? (string-length line))
                      (char=? (string-ref line 0) #\;)
                      (char=? (string-ref line 0) #\#))
                  (loop sect tbl)]
                 [(and (>= (string-length line) 2)
                       (char=? (string-ref line 0) #\[)
                       (char=? (string-ref line (- (string-length line) 1)) #\]))
                  (let* ((name (substring line 1 (- (string-length line) 1)))
                         (tbl* (if (assoc name tbl) tbl (cons (cons name '()) tbl))))
                    (loop name tbl*))]
                 [else
                  (let* ((clean (strip-inline-comment line))
                         (eqi   (string-index clean #\=))
                         (k     (string-trim-both (substring clean 0 eqi)))
                         (v     (string-trim-both (substring clean (+ eqi 1))))
                         (sect-al (or (and sect (assoc-ref tbl sect)) '()))
                         (sect-al* (alist-set sect-al k v))
                         (tbl* (alist-set tbl sect sect-al*)))
                    (loop sect tbl*))]))))))))

(define (cfg-get ini section key)
  (let* ((s (assoc section ini))
         (kv (and s (assoc key (cdr s)))))
    (and kv (cdr kv))))

(define password-characters
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-=+_")

(define (get-string-value s)
  (let loop ((i 0) (acc 0))
    (if (= i (string-length s))
        acc
        (loop (+ i 1) (+ acc (char->integer (string-ref s i)))))))

(define (count-char c s)
  (let loop ((i 0) (n 0))
    (if (= i (string-length s))
        n
        (loop (+ i 1) (+ n (if (char=? c (string-ref s i)) 1 0))))))

(define* (generate-password username website password master-key pre-key length pre-key-1 pre-key-2
                            #:key (init-key 1))
  (let* ((pre (if (= init-key 1)
                  (string-append pre-key-1 pre-key)
                  (string-append pre-key-2 pre-key)))
         (username* (string-append pre username))
         (website*  (string-downcase (string-append pre website)))
         (len       (inexact->exact (or (and (number? length) length)
                                        (string->number (or length "0"))
                                        0)))
         (u-val (get-string-value username*))
         (p-val (get-string-value password))
         (m-val (get-string-value master-key))
         (w-val (get-string-value website*))
         (sumv  (* u-val p-val m-val w-val))
         (chars password-characters)
         (chars-len (string-length chars)))
    (define (random-char idx k)
      (let* ((u (char->integer (string-ref username* (modulo idx (string-length username*)))))
             (p (char->integer (string-ref password   (modulo idx (string-length password)))))
             (m (char->integer (string-ref master-key (modulo idx (string-length master-key)))))
             (w (char->integer (string-ref website*   (modulo idx (string-length website*)))))
             (r (+ (* u (+ sumv k))
                   (* p (+ sumv k))
                   (* m (+ sumv k))
                   (* w (+ sumv k)))))
        (string (string-ref chars (modulo r chars-len)))))
    (let build ((i 1) (acc ""))
      (if (> i len)
          (let ((pwd acc))
            (if (and (> (string-length pwd) 0)
                     (> (count-char (string-ref pwd 0) pwd) 10))
                (begin
                  (format #t "generating another password, ~a~%" pwd)
                  (generate-password username website password master-key pre-key length
                                     pre-key-1 pre-key-2 #:init-key (+ init-key 1)))
                pwd))
          (build (+ i 1) (string-append acc (random-char i 0)))))))

(define CONFIG-DIR
  (let ((home (or (getenv "HOME") "")))
    ;; mirror your original absolute path; falls back to HOME if present
    (or "/home/ebeem/.config/passwords-manager/"
        (string-append home "/.config/passwords-manager/"))))

(define (read-preferences)
  (let* ((ini (parse-ini (string-append CONFIG-DIR "config.ini"))))
    `((password   . ,(or (cfg-get ini "main" "password") ""))
      (master_key . ,(or (cfg-get ini "main" "master_key") ""))
      (pre_key    . ,(or (cfg-get ini "main" "pre_key") ""))
      (pre_key_1  . ,(or (cfg-get ini "main" "pre_key_1") ""))
      (pre_key_2  . ,(or (cfg-get ini "main" "pre_key_2") ""))
      (length     . ,(let ((v (cfg-get ini "main" "length")))
                       (or (and v (string->number v)) 0))))))

(define settings (read-preferences))

(define (build-menu)
  (let ((ini (parse-ini (string-append CONFIG-DIR "accounts.ini"))))
    (append-map
     (lambda (sec-pair)
       (let ((section (car sec-pair))
             (opts    (cdr sec-pair)))           ; alist of (key . value)
         (map (lambda (kv)
                (string-append section "@" (cdr kv)))
              opts)))
     ini)))

(define (get-password website username)
  (generate-password username
                     website
                     (assoc-ref settings 'password)
                     (assoc-ref settings 'master_key)
                     (assoc-ref settings 'pre_key)
                     (assoc-ref settings 'length)
                     (assoc-ref settings 'pre_key_1)
                     (assoc-ref settings 'pre_key_2)))

(define (main-terminal)
  (let* ((args (command-line)))
    (if (> (length args) 1)
        (let* ((account (cadr args))
               (at (string-index account #\@)))
          (if at
              (let ((website (substring account 0 at))
                    (username (substring account (+ at 1) (string-length account))))
                (format #t "~a~%" (get-password website username)))
              (begin
                (format #t "Usage: ~a SECTION@VALUE\n" (car args))
                (exit 1))))
		(for-each (lambda (s) (format #t "~a~%" s))
          (build-menu)))))

(define (main)
  (let* ((account (dmenu-prompt "Choose account > "
							   (build-menu))))
	(let* ((at (string-index account #\@)))
	  (if at
		  (let ((website (substring account 0 at))
				(username (substring account (+ at 1) (string-length account))))
			(format #t "~a~%" (get-password website username)))))))

(main)
