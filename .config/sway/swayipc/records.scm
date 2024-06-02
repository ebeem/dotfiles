(define-module (swayipc records)
  #:use-module (oop goops)
  #:use-module (json)

  #:export (<sway-success>
            scm->sway-success
            json->sway-success
            sway-success-success
            sway-success-parse-error
            sway-success-error
            ))

(define-json-type <sway-success>
  (success)
  (parse-error "parse_error")
  (error))
