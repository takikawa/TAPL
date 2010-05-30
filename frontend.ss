#lang scheme

(require "ast.ss")
(require "error.ss")
(require "eval.ss")
(require "parser.ss")

;; repl : void
(define (repl)
  (prompt)
  (let ([line (read-line)])
    (with-handlers ([exn:fail:user:lam?
                     (lambda (exn) 
                       (printf "Error: ~a" (exn-message exn)))])
      ((compose print
                unwrap
                eval
                relabel
                parse
                create-lexer
                open-input-string)
       line))
    (newline)
    (repl)))

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