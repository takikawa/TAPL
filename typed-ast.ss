#lang typed-scheme

(provide
 Program?
 TopLevel?
 (struct-out Def)
 (struct-out Exp)
 (struct-out ValueExp)
 (struct-out LamExp)
 (struct-out ArithExp)
 (struct-out ApplExp)
 (struct-out VarExp)
 (struct-out NumExp)
 (struct-out TrueExp)
 (struct-out FalseExp)
 (struct-out Binding)
 (struct-out Type)
 (struct-out NumType)
 (struct-out BoolType)
 (struct-out ArrowType))

;; utility functions for printing types
(: print-type (Type Output-Port Boolean -> Void))
(define (print-type type port write?)
  (let ([type-str (type->string type)])
    ((if write? write display) type-str port)))

(: type->string (Type -> String))
(define (type->string type)
  (cond [(NumType? type) "Number"]
        [(BoolType? type) "Bool"]
        [(ArrowType? type)
         (string-append "("
                        (type->string (ArrowType-t1 type))
                        "->"
                        (type->string (ArrowType-t2 type))
                        ")")]
        [else 
         (error "Unimplemented type")]))

;; Grammar with types: adds type annotations to the language
;; which are erased to the normal grammar by the type checker
;;
;; the grammar of this language is basically
;; program ::= top-level program | eof
;; top-level ::= exp | definition
;; definition ::= (var : type) := exp
;; exp ::= lambda | arithmetic | variable | values | application
;; values ::= nat | bool (defined in the obvious ways)
;; arithmetic ::= (exp + exp) | (exp - exp)
;; lambda ::= (λ (var : type) exp)
;; application ::= (exp exp) 
;; type ::= number | boolean | type -> type
;;
(define-type-alias Program (Listof TopLevel))
(define-type-alias TopLevel (U Def Exp))
(define-struct: Def ([var : Binding]
                     [body : Exp])
  #:transparent)
(define-struct: Exp ()                           
  #:transparent)
(define-struct: (ValueExp Exp) ()               
  #:transparent)
(define-struct: (LamExp ValueExp) ([var : Binding]
                                   [type : Type]
                                   [body : Exp])
  #:transparent)
(define-struct: (ArithExp Exp) ([op : Symbol]
                                [rand1 : Exp] 
                                [rand2 : Exp]) 
  #:transparent)
(define-struct: (ApplExp Exp) ([e1 : Exp] 
                               [e2 : Exp])
  #:transparent)
(define-struct: (VarExp Exp) ([name : String])
  #:transparent)
(define-struct: (NumExp ValueExp) ([val : Number])
  #:transparent)
(define-struct: (TrueExp ValueExp) ()
  #:transparent)
(define-struct: (FalseExp ValueExp) ()
  #:transparent)
(define-struct: Binding ([var : String]
                         [type : Type])
  #:transparent)
(define-struct: Type ()
  #:transparent)
(define-struct: (NumType Type) ()
  #:transparent
  #:property prop:custom-write print-type)
(define-struct: (BoolType Type) ()
  #:transparent
  #:property prop:custom-write print-type)
(define-struct: (ArrowType Type) ([t1 : Type]
                                  [t2 : Type])
  #:transparent
  #:property prop:custom-write print-type)

(define-predicate Program? Program)
(define-predicate TopLevel? TopLevel)