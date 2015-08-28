#lang typed/racket/base
;
; Utilities for Error Reporting
;

(provide fail)


(: fail (-> String String * Nothing))
(define (fail fmt . args)
  (parameterize ((current-output-port (current-error-port)))
    (write-string (apply format fmt args))
    (newline (current-error-port)))
  (exit 1))


; vim:set ts=2 sw=2 et:
