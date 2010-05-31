#lang scheme

(require "ast.ss")
(require "error.ss")
(require "parser.ss")

(require schemeunit)
(require schemeunit/gui)

(provide/contract
 [relabel     (-> (or/c exp? def?) (or/c exp? def?))]
 [remove-defs (-> (listof (or/c exp? def?)) (listof exp?))]
 [eval-prog   (-> (listof (or/c exp? def?)) hash? value-exp?)])

(provide run-eval-tests)

;; the environment is a hashtable
;; env? : env -> boolean
(define env? hash?)
;; make-env : env
(define (make-env)
  (make-immutable-hash null))
;; add-env : env string exp -> env
(define add-env hash-set)
;; get-env : env string -> exp or #f
(define (get-env env str)
  (hash-ref env str #f))

;; provide environment datatype and related operations
(provide/contract
 [env?       (-> any/c boolean?)]
 [make-env   (-> env?)]
 [build-env  (-> (listof (or/c exp? def?)) env?)]
 [add-to-env (-> (listof (or/c exp? def?)) env? env?)])

;; relabel : top-level -> top-level
;; converts variables names to debruijn indices
(define (relabel exp)
  (let loop ([exp exp] [labels empty])
    (match exp
      [(struct lam-exp (var body))
       (make-lam-exp #f 
                     (loop body 
                           (add-label var (incr-labels labels))))]
      [(struct appl-exp (e1 e2))
       (make-appl-exp (loop e1 labels) (loop e2 labels))]
      [(struct arith-exp (op r1 r2))
       (make-arith-exp op (loop r1 labels) (loop r2 labels))]
      [(struct var-exp (var))
       (let ([v-label (get-label var labels)])
         (make-var-exp (if v-label
                           v-label
                           var)))]
      [(struct def (var body))
       (make-def var (relabel body))]
      [other other])))

;; add-label : labels -> labels
;; add a new index to the label set
(define (add-label var labels)
  (cons (cons var 0) labels))

;; get-label : string labels -> index or #f
(define (get-label var labels)
  (let ([result (assoc var labels)])
    (if result
        (cdr result)
        #f)))

;; incr-labels : labels -> labels
;; increment debruin indices
(define (incr-labels labels)
  (map (lambda (p)
         (cons (car p) (add1 (cdr p))))
       labels))

(check-equal? (incr-labels (list (cons "x" 1) (cons "y" 2)))
              (list (cons "x" 2) (cons "y" 3)))

;; shift : exp number -> exp
;; shift indices by the required amount in the De Bruijn rep.
(define (shift exp incr)
  (let loop ([exp exp] [incr incr] [depth 0])
    (match exp
      [(struct lam-exp (var body))
       (make-lam-exp var (loop body incr (add1 depth)))]
      [(struct appl-exp (e1 e2))
       (make-appl-exp (loop e1 incr depth) (loop e2 incr depth))]
      [(struct arith-exp (op r1 r2))
       (make-arith-exp op (loop r1 incr depth) (loop r2 incr depth))]
      [(struct var-exp (idx))
       (if (and (number? idx)
                (> idx depth))
           (make-var-exp (+ idx incr))
           (make-var-exp idx))]
      [(struct value-exp ())
       exp]
      [_ (raise-lam-error "Index shifting failed: no match")])))

;; beta-subst : exp exp -> exp
;; implements beta substition for this lambda calculus
(define (beta-subst lam arg)
  (shift 
   (let loop ([exp (lam-exp-body lam)] [arg arg] [depth 0])
     (match exp
       [(struct lam-exp (var body))
        (make-lam-exp var (loop body arg (add1 depth)))]
       [(struct appl-exp (e1 e2))
        (make-appl-exp (loop e1 arg depth)
                       (loop e2 arg depth))]
       [(struct arith-exp (op r1 r2))
        (make-arith-exp op 
                        (loop r1 arg depth)
                        (loop r2 arg depth))]
       [(struct var-exp (var))
        (if (equal? depth var)
            (shift arg (add1 depth))
            exp)]
       [(struct value-exp ())
        exp]
       [_ (raise-lam-error "Beta substition failed: no match")]))
   -1))

;; build-env : program -> (program, env)
;; scan the program for definitions
(define (build-env prog)
  (add-to-env prog (make-env)))

(define (add-to-env prog env)
  (foldl process-top-level env prog))
  
(define (process-top-level top env)
  (match top
    [(struct exp ()) env]
    [(struct def (var body)) 
     (add-env env var body)]))

;; remove-defs : program -> program
;; remove definitions from the program after building the env
(define (remove-defs prog)
  (filter (lambda (t) (exp? t)) prog))
      
;; eval : exp env -> val-exp
;; evaluates the result of a single well-formed lambda expression/statement
(define (eval exp env)
  (match exp
    [(struct appl-exp ((struct lam-exp (_ _)) e2))
     (eval (beta-subst (appl-exp-e1 exp) e2) env)]
    [(struct appl-exp ((struct var-exp (var)) e2))
     (let ([binding (get-env env var)])
       (if (exp? binding)
           (eval (make-appl-exp binding e2) env)
           (raise-lam-error "Found unbound operator/function")))]
    [(struct appl-exp (v1 e2)) 
     (=> fail)
     (if (not (value-exp? v1))
         (fail)
         (make-appl-exp v1 (eval e2 env)))]
    [(struct appl-exp (e1 v2)) 
     (=> fail)
     (if (not (value-exp? v2))
         (fail)
         (make-appl-exp (eval e1 env) v2))]
    [(struct arith-exp ('plus r1 r2))
     (make-num-exp
      (+ (num-exp-val (eval r1 env)) 
         (num-exp-val (eval r2 env))))]
    [(struct arith-exp ('minus r1 r2))
     (make-num-exp
      (- (num-exp-val (eval r1 env)) 
         (num-exp-val (eval r2 env))))]
    [(struct value-exp ())
     exp]
    [(struct var-exp (var))
     (let ([binding (get-env env var)])
       (if (exp? binding)
           (eval binding env)
           (raise-lam-error "Free variable")))]
    [_ (raise-lam-error "No matching clause in eval")]))

;; eval-prog : program env -> val-exp
;; evaluate a whole program
(define (eval-prog prog env)
  (cond [(null? prog)
         (make-true-exp)]
        [(null? (rest prog))
         (eval (first prog) env)]
        [else (begin (eval (first prog) env)
                     (eval-prog (rest prog) env))]))

;; Tests!

;; De Bruijn examples
(define K
  (make-lam-exp 
   "x"
   (make-lam-exp 
    "y"
    (make-var-exp "x"))))

(define K-nameless
  (make-lam-exp 
   #f
   (make-lam-exp 
    #f
    (make-var-exp 1))))

(define S
  (make-lam-exp 
   "x"
   (make-lam-exp 
    "y"
    (make-lam-exp 
     "z"
     (make-appl-exp
      (make-appl-exp 
       (make-var-exp "x")
       (make-var-exp "z"))
      (make-appl-exp
       (make-var-exp "y")
       (make-var-exp "z")))))))

(define S-nameless
  (make-lam-exp 
   #f
   (make-lam-exp 
    #f
    (make-lam-exp 
     #f
     (make-appl-exp
      (make-appl-exp 
       (make-var-exp 2)
       (make-var-exp 0))
      (make-appl-exp
       (make-var-exp 1)
       (make-var-exp 0)))))))

;; test helpers
(define-syntax test-beta
  (syntax-rules ()
    [(test-beta exp arg)
     (beta-subst (relabel exp) arg)]))

(define-syntax test-eval
  (syntax-rules ()
    [(test-eval str)
     (let ([prog (map relabel (parse (create-lexer (open-input-string str))))])
       (eval-prog (remove-defs prog)
                  (build-env prog)))]))

;; test suites
(define renaming-tests
  (test-suite
   "De Bruijn Indexing"
   (test-case
    "K combinator"
    (check-equal? (relabel K) K-nameless))
   (test-case
    "S combinator"
    (check-equal? (relabel S) S-nameless))))

(define shift-tests
  (test-suite
   "Variable shifting"
   (test-case
    "No shifting"
    (check-equal? (shift (make-lam-exp #f (make-var-exp 0)) 1)
                  (make-lam-exp #f (make-var-exp 0))))
   (test-case
    "Shift needed"
    (check-equal? (shift (make-lam-exp #f (make-var-exp 3)) -1)
                  (make-lam-exp #f (make-var-exp 2))))))

(define beta-subst-tests
  (test-suite
   "Beta Substitution"
   (test-case
    "K combinator"
    (check-equal? (test-beta K (make-num-exp 5))
                  (make-lam-exp 
                   #f
                   (make-num-exp 5))))
   (test-case
    "Index shifting 1"
    (check-equal? (beta-subst (make-lam-exp 
                               #f
                               (make-appl-exp
                                (make-var-exp 2)
                                (make-appl-exp
                                 (make-var-exp 0)
                                 (make-var-exp 1))))
                              (make-lam-exp
                               #f
                               (make-appl-exp
                                (make-var-exp 1)
                                (make-var-exp 0))))
                  (make-appl-exp
                   (make-var-exp 1)
                   (make-appl-exp
                    (make-lam-exp
                     #f
                     (make-appl-exp
                      (make-var-exp 1)
                      (make-var-exp 0)))
                    (make-var-exp 0)))))))

;; utilities to check exceptions
(define test-free-var-exn
  (lambda (exn) 
    (and (exn:fail:user:lam? exn)
         (equal? (exn-message exn)
                 "Free variable"))))

(define eval-tests
  (test-suite
   "Evaluation"
   (test-case
    "Undefined identifier"
    (check-exn test-free-var-exn
               (lambda () (test-eval "foobar"))))
   (test-case
    "Simple lambda with arithmetic"
    (check-equal? (test-eval "((lambda x (x + 1)) 5)")
                  (make-num-exp 6)))
   (test-case
    "Lambda application with free variable"
    (check-exn test-free-var-exn
               (lambda () 
                 (test-eval "((lambda x (y + 5)) 5)"))))
   (test-case
    "Constant definition"
    (check-equal? (test-eval "foo := (1 + 3) foo")
                  (make-num-exp 4)))
   (test-case
    "Simple function definition"
    (check-equal? (test-eval "add1 := (lambda x (x + 1))\n(add1 5)")
                  (make-num-exp 6)))))

(define run-eval-tests
  (lambda () (test/gui renaming-tests
                       shift-tests
                       beta-subst-tests
                       eval-tests)))