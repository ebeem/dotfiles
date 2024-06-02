(define-module (hypr info)
  #:use-module (hypr records)
  #:use-module (hypr utility)
  #:use-module (oop goops)
  #:use-module (json)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (hypr-info-version
            hypr-info-monitors
            hypr-info-workspaces
            hypr-info-active-workspace))

(define (hypr-info-version)
  (json->hypr-version (hypr-command "version")))

(define (hypr-info-workspaces)
  (map
    (lambda (workspace)
      (scm->hypr-workspace workspace))
    (vector->list
     (json-string->scm
      (hypr-command "workspaces")))))

(define (hypr-info-monitors)
  (map
    (lambda (monitor)
      (scm->hypr-monitor monitor))
    (vector->list
     (json-string->scm
      (hypr-command "monitors")))))

(define (hypr-info-active-workspace)
  (json->hypr-workspace (hypr-command "activeworkspace")))
