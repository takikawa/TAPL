#lang scheme

(require "ast.ss")
(require "error.ss")
(require "parser.ss")

(require schemeunit)
(require schemeunit/gui)

(provide/contract
 [relabel (-> exp? exp?)]
 [eval    (-> exp? value-exp?)])

(provide run-eval-tests)

;; relabel : exp -> exp
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
      
;; eval
;; evaluates the result of a well-formed lambda program
(define (eval exp)
  (match exp
    [(struct appl-exp ((struct lam-exp (_ _)) e2))
     (eval (beta-subst (appl-exp-e1 exp) e2))]
    [(struct appl-exp (v1 e2)) 
     (=> fail)
     (if (not (value-exp? v1))
         (fail)
         (make-appl-exp v1 (eval e2)))]
    [(struct appl-exp (e1 v2)) 
     (=> fail)
     (if (not (value-exp? v2))
         (fail)
         (make-appl-exp (eval e1) v2))]
    [(struct arith-exp ('plus r1 r2))
     (make-num-exp
      (+ (num-exp-val (eval r1)) 
         (num-exp-val (eval r2))))]
    [(struct arith-exp ('minus r1 r2))
     (make-num-exp
      (- (num-exp-val (eval r1)) 
         (num-exp-val (eval r2))))]
    [(struct value-exp ())
     exp]
    [(struct var-exp (var))
     (raise-lam-error "Free variable")]
    [_ (raise-lam-error "No matching clause in eval")]))

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
     (eval (relabel (parse (create-lexer (open-input-string str)))))]))

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
                 (test-eval "((lambda x (y + 5)) 5)"))))))

(define run-eval-tests
  (lambda () (test/gui renaming-tests
                       shift-tests
                       beta-subst-tests
                       eval-tests)))