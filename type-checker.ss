#lang scheme

(require (prefix-in untyped/ "ast.ss"))
(require "error.ss")
(require "typed-ast.ss")

(require schemeunit)
(require schemeunit/gui)

;; the environment is a hashtable
;; env? : env -> boolean
(define env? hash?)
;; make-env : env
(define (make-env)
  (make-immutable-hash null))
;; add-env : env string type -> env
(define add-env hash-set)
;; get-env : env string -> type or #f
(define (get-env env str)
  (hash-ref env str #f))

;; type-check: TopLevel -> (Type untyped/TopLevel)
;; check the type. if successful return the type and erased code
(define (type-check top)
  (let loop ([top top] [env (make-env)])
    (match top 
      ;; TODO: type checker should track definition bindings in type checking
      [(struct Def ((struct Binding (name expt)) body))
       (let-values ([(actl e) (loop body env)])
         (if (equal? actl expt)
             (values expt (untyped/make-def name e))
             (raise-type-error expt actl)))]
      [(struct LamExp ((struct Binding (name in-type)) out-type body))
       (let-values ([(actl e) (loop body (add-env env name in-type))])
         (if (equal? actl out-type)
             (values (make-ArrowType in-type out-type) (untyped/make-lam-exp name e))
             (raise-lam-type-error out-type actl)))]
      [(struct ApplExp (e1 e2))
       (let-values ([(a1 ersd1) (loop e1 env)]
                    [(a2 ersd2) (loop e2 env)])
         (if (ArrowType? a1)
             (if (equal? (ArrowType-t1 a1) a2)
                 (values (ArrowType-t2 a1) (untyped/make-appl-exp ersd1 ersd2))
                 (raise-lam-type-error (ArrowType-t1 a1) a2))
             (raise-lam-type-error (format "Expected lambda expression but got ~a" a1))))]
      [(struct ArithExp (op r1 r2))
       (let-values ([(a1 e1) (loop r1 env)]
                    [(a2 e2) (loop r2 env)])
         (if (NumType? a1)
             (if (NumType? a2)
                 (values (make-NumType) (untyped/make-arith-exp op e1 e2))
                 (raise-lam-type-error (make-NumType) a2))
             (raise-lam-type-error (make-NumType) a1)))]
      [(struct NumExp (val))
       (values (make-NumType) (untyped/make-num-exp val))]
      [(struct TrueExp ())
       (values (make-BoolType) (untyped/make-true-exp))]
      [(struct FalseExp ())
       (values (make-BoolType) (untyped/make-false-exp))]
      [(struct VarExp (var))
       (values (get-env env var) (untyped/make-var-exp var))]
      [other (error "Unimplemented typechecking case" top)])))

;; erase: erase types from the program if type check succeeds
(provide/contract
 [erase (-> Program? (listof (or/c untyped/def? untyped/exp?)))])
         
(define erase
   (curry map (lambda (x) 
                (call-with-values (lambda () (type-check x))
                                  (lambda (t e) e)))))

;; Tests
(define-syntax test-type-check
  (syntax-rules ()
    [(test-type-check exp type-val ast-val)
     (let-values ([(type ast) (type-check exp)])
       (check-equal? type type-val)
       (check-equal? ast ast-val))]))

(define LAM1 (make-LamExp (make-Binding "x" (make-BoolType))
                          (make-NumType)
                          (make-ArithExp '+ 
                                         (make-NumExp 6)
                                         (make-NumExp 3))))
(define untyped/LAM1 (untyped/make-lam-exp 
                      "x"
                      (untyped/make-arith-exp 
                       '+
                       (untyped/make-num-exp 6)
                       (untyped/make-num-exp 3))))

(define TYPE1 (make-ArrowType (make-NumType) (make-BoolType)))
(define TYPE2 (make-ArrowType (make-BoolType) (make-NumType)))
(define LAM2 (make-LamExp (make-Binding "x" TYPE1) 
                          TYPE2
                          (make-LamExp (make-Binding "y" (make-BoolType))
                                       (make-NumType)
                                       (make-NumExp 3))))
(define untyped/LAM2 (untyped/make-lam-exp "x" 
                                      (untyped/make-lam-exp "y"
                                                       (untyped/make-num-exp 3))))
(define LAM3 (make-LamExp (make-Binding "z" (make-NumType))
                          (make-BoolType)
                          (make-TrueExp)))
(define untyped/LAM3 (untyped/make-lam-exp "z" (untyped/make-true-exp)))
(define APPL-GOOD (make-ApplExp LAM2 LAM3))
(define untyped/APPL-GOOD (untyped/make-appl-exp untyped/LAM2 untyped/LAM3))
(define APPL-BAD  (make-ApplExp LAM2 (make-TrueExp)))

(define type-check-tests
  (test-suite
   "Type checking"
   (test-case
    "Number type"
    (test-type-check (make-NumExp 5) (make-NumType) (untyped/make-num-exp 5)))
   (test-case
    "Boolean type"
    (test-type-check (make-TrueExp) (make-BoolType) (untyped/make-true-exp)))
   (test-case
    "Lambda expression"
    (test-type-check LAM1 (make-ArrowType (make-BoolType)
                                          (make-NumType))
                     untyped/LAM1))
   (test-case
    "Good application"
    (test-type-check (make-ApplExp LAM1 (make-TrueExp))
                     (make-NumType)
                     (untyped/make-appl-exp untyped/LAM1 
                                            (untyped/make-true-exp))))
   (test-case
    "Bad application"
    (check-exn exn:fail:user:lam:type?
               (lambda () 
                 (type-check (make-ApplExp LAM1 (make-NumExp 5))))))
   (test-case
    "Higher order function - Good"
    (test-type-check APPL-GOOD TYPE2 untyped/APPL-GOOD))
   (test-case
    "Higher order function - Bad"
    (check-exn exn:fail:user:lam:type?
               (lambda () (type-check APPL-BAD))))))

(define run-type-tests
  (lambda ()
    (test/gui type-check-tests)))