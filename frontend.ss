#lang scheme

(require "ast.ss")
(require "error.ss")
(require "eval.ss")
(require "parser.ss")

;; repl : void
(define (repl)
  (let loop ([env (make-env)])
    (prompt)
    (let ([line (read-line)])
      (let ([new-env 
             (with-handlers ([exn:fail:user:lam?
                              (lambda (exn) 
                                (printf "Error: ~a" (exn-message exn)))])
               ((compose (lambda (v e) (begin (print v) e))
                         (lambda (v e) (values (unwrap v) e))
                         (lambda (p e) (values (eval-prog p e) e))
                         (lambda (p e) (values (remove-defs p) e))
                         (lambda (p) (values p (add-to-env p env)))
                         (curry map relabel)
                         parse
                         create-lexer
                         open-input-string)
                line))])
        (newline)
        (loop new-env)))))

;; unwrap : exp -> number or boolean or string
;; unwrap AST result into a scheme result
(define (unwrap exp)
  (match exp
    [(struct lam-exp (_ _))
     "lambda abstraction..."]
    [(struct true-exp ())
     #t]
    [(struct false-exp ())
     #f]
    [(struct num-exp (val))
     val]
    [_ (raise-lam-error "Unknown value after eval, cannot unwrap")]))

(define (prompt)
  (display ":> "))

(repl)