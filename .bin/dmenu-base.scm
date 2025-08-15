#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
			 (srfi srfi-18)
			 (srfi srfi-9)
			 (srfi srfi-26)
			 (srfi srfi-1))

(define* (run-command cmd #:key (echo? #f) (port (current-error-port)))
  (let* ((pipe  (open-input-pipe cmd))
         (lines (let loop ((acc '()))
                  (let ((line (read-line pipe)))
                    (if (eof-object? line)
                        (reverse acc)
                        (begin
                          (when echo?
                            (display line port)
                            (newline port)
                            (force-output port))
                          (loop (cons line acc))))))))
    (close-pipe pipe)
    lines))

(define* (run-command-async cmd #:key (echo? #f) (port (current-error-port)))
  (let ((t (make-thread
            (lambda ()
              (let* ((pipe  (open-input-pipe cmd))
                     (lines (let loop ((acc '()))
                              (let ((line (read-line pipe)))
                                (if (eof-object? line)
                                    (reverse acc)
                                    (begin
                                      (when echo?
                                        (display line port)
                                        (newline port)
                                        (force-output port))
                                      (loop (cons line acc))))))))
                (close-pipe pipe)
                lines))
            (string-append "run-command-async: " cmd))))
    (thread-start! t)
    t))

(define (await-command th)
  (thread-join! th))

(define (command-finished? th)
  (eq? 'terminated (thread-status th)))

(define (dmenu-prompt prompt options)
  (let* ((input (open-input-pipe
                 (string-append "echo \"" (string-join options "\n") "\" | fuzzel --dmenu -p \"" prompt "\"")))
         (choice (read-line input)))
    (close-pipe input)
    (if (eof-object? choice) "" choice)))

