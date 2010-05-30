#lang typed-scheme

(provide
 (struct-out exp)
 (struct-out value-exp)
 (struct-out lam-exp)
 (struct-out arith-exp)
 (struct-out appl-exp)
 (struct-out var-exp)
 (struct-out num-exp)
 (struct-out true-exp)
 (struct-out false-exp))

;; parser and AST data definitions
;;
;; the grammar of this language is basically
;; exp ::= lambda | arithmetic | variable | values | application
;; values ::= nat | bool (defined in the obvious ways)
;; arithmetic ::= (exp + exp) | (exp - exp)
;; lambda ::= λ string exp
;; application ::= (exp exp) 
;;
(define-struct: exp ()                           
  #:transparent)
(define-struct: (value-exp exp) ()               
  #:transparent)
(define-struct: (lam-exp value-exp) ([var : (U String Boolean)]
                                     [body : exp])   
  #:transparent)
(define-struct: (arith-exp exp) ([op : Symbol]
                                 [rand1 : exp] 
                                 [rand2 : exp]) 
  #:transparent)
(define-struct: (appl-exp exp) ([e1 : exp] 
                                [e2 : exp])
  #:transparent)
(define-struct: (var-exp exp) ([name : (U String Number)])
  #:transparent)
(define-struct: (num-exp value-exp) ([val : Number])
  #:transparent)
(define-struct: (true-exp value-exp) ()
  #:transparent)
(define-struct: (false-exp value-exp) ()
  #:transparent)