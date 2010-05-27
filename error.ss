#lang scheme

(provide (struct-out exn:fail:user:lam))

(provide/contract
 [raise-lam-error (-> string? any)])

(define-struct (exn:fail:user:lam exn:fail:user) ()
  #:transparent)

(define (raise-lam-error msg)
  (raise (make-exn:fail:user:lam 
          msg 
          (current-continuation-marks))))