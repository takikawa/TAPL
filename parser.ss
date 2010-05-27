#lang scheme

(require "error.ss")

(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))

(require schemeunit)
(require schemeunit/gui)

;; parser and AST data definitions
;;
;; the grammar of this language is basically
;; exp ::= lambda | arithmetic | variable | values | application
;; values ::= nat | bool (defined in the obvious ways)
;; arithmetic ::= (exp + exp) | (exp - exp)
;; lambda ::= λ string exp
;; application ::= (exp exp) 
;;
(define-struct exp ()                           #:transparent)
(define-struct (value-exp exp) ()               #:transparent)
(define-struct (lam-exp value-exp) (var body)   #:transparent)
(define-struct (arith-exp exp) (op rand1 rand2) #:transparent)
(define-struct (appl-exp exp) (e1 e2)           #:transparent)
(define-struct (var-exp exp) (name)             #:transparent)
(define-struct (num-exp value-exp) (val)        #:transparent)
(define-struct (true-exp value-exp) ()          #:transparent)
(define-struct (false-exp value-exp) ()         #:transparent)

(provide/contract 
 [create-lexer (-> input-port? (-> (or/c symbol? token?)))]
 [parse        (-> (-> (or/c symbol? token?)) exp?)])

(provide run-parser-tests)

(provide (struct-out exp) 
         (struct-out value-exp)
         (struct-out lam-exp) 
         (struct-out arith-exp)
         (struct-out appl-exp) 
         (struct-out var-exp)
         (struct-out num-exp)
         (struct-out true-exp)
         (struct-out false-exp))

;; define tokens for lambda language
(define-tokens lam-var-toks
  (var nat))
(define-empty-tokens lam-toks
  (lam plus minus true false lpar rpar end))

;; lex-once : input-port -> token
;; takes a portion of input and produces the token matched
(define lex-once
  (lexer [(eof) (token-end)]
         [whitespace (lex-once input-port)]
         ["lambda" (token-lam)]
         ["+"      (token-plus)]
         ["-"      (token-minus)]
         ["true"   (token-true)]
         ["false"  (token-false)]
         [#\(      (token-lpar)]
         [#\)      (token-rpar)]
         [(:: alphabetic (:* (:or alphabetic digit)))
          (token-var lexeme)]
         [lex-numeric
          (token-nat (string->number lexeme))]))

;; utility lex abbreviations
(define-lex-abbrev digit
  (:or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define-lex-abbrev lex-numeric
  (:+ digit))

;; create-lexer : input-port -> (-> token?)
(define (create-lexer input)
  (lambda () (lex-once input)))

;; lex : input-port -> (listof tokens)
;; consume the entire input-port and return the tokens
(define (lex input)
  (let ([result (lex-once input)])
    (if (eq? (token-name result) 'end)
        empty
        (cons result (lex input)))))

;; parse : (-> token?) -> exp?
;; parse the input and return an AST
(define parse
  (parser 
   (grammar (exp
             [(lam-exp) $1]
             [(arith-exp) $1]
             [(var) (make-var-exp $1)]
             [(nat) (make-num-exp $1)]
             [(bool-exp) $1]
             [(appl-exp) $1])
            (lam-exp
             [(lam var exp)
              (make-lam-exp $2 $3)])
            (arith-exp
             [(lpar exp plus exp rpar)
              (make-arith-exp 'plus $2 $4)]
             [(lpar exp minus exp rpar) 
              (make-arith-exp 'minus $2 $4)])
            (bool-exp
             [(true) (make-true-exp)]
             [(false) (make-false-exp)])
            (appl-exp
             [(lpar exp exp rpar)
              (make-appl-exp $2 $3)]))
   (tokens lam-toks lam-var-toks)
   (error (lambda (tok-ok? tok-name tok-value) 
            (raise-lam-error "Parsing failed on token"))) 
   (start exp)
   (end end)))

;; Test suites for parsing and lexing

;; syntax macro for testing
(define-syntax test-lex
  (syntax-rules ()
    [(test-lex str) (lex (open-input-string str))]))

(define-syntax test-parse
  (syntax-rules ()
    [(test-parse str) (parse (create-lexer (open-input-string str)))]))

;; simple lexing tests
(define-test-suite simple-lexing-tests
  (check-equal? (lex-once (open-input-string "")) (token-end))
  (check-equal? (lex-once (open-input-string "1")) (token-nat 1))
  (check-equal? (lex-once (open-input-string "+")) (token-plus))
  (check-equal? (lex-once (open-input-string "-")) (token-minus))
  (check-equal? (lex-once (open-input-string "x23")) (token-var "x23"))
  (check-equal? (lex-once (open-input-string "lambda")) (token-lam))
  (check-equal? (lex-once (open-input-string "true")) (token-true))
  (check-equal? (lex-once (open-input-string "false")) (token-false))
  (check-equal? (lex-once (open-input-string "true false")) (token-true)))

;; full lexing tests
(define-test-suite full-lexing-tests
  (check-equal? (test-lex "true false")
                (list (token-true) (token-false)))
  (check-equal? (test-lex "1 + 1")
                (list (token-nat 1) (token-plus) (token-nat 1)))
  (check-equal? (test-lex "lambda x (1 + x)")
                (list (token-lam) (token-var "x") (token-lpar)
                      (token-nat 1) (token-plus) (token-var "x")
                      (token-rpar))))

;; examples for test suites
(define lam-ex
  (make-lam-exp
   "x"
   (make-arith-exp 'plus 
                   (make-num-exp 1)
                   (make-var-exp "x"))))

;; parsing tests
(define parsing-tests
  (test-suite 
   "Parsing test suite"
   (test-case
    "Number value"
    (check-equal? (test-parse "5")
                  (make-num-exp 5)))
   (test-case
    "Lambda expression"
    (check-equal? (test-parse "lambda x (1 + x)")
                  lam-ex))
   (test-case
    "Application"
    (check-equal? (test-parse "(lambda x (1 + x) 5)")
                  (make-appl-exp lam-ex
                                 (make-num-exp 5))))))

(define run-parser-tests
  (lambda () (test/gui simple-lexing-tests 
                       full-lexing-tests
                       parsing-tests)))