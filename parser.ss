#lang scheme

(require "ast.ss")
(require "error.ss")

(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))

(require schemeunit)
(require schemeunit/gui)

(provide/contract 
 [create-lexer (-> input-port? (-> (or/c symbol? token?)))]
 [parse        (-> (-> (or/c symbol? token?)) (listof (or/c exp? def?)))])

(provide run-parser-tests)

;; define tokens for lambda language
(define-tokens lam-var-toks
  (var nat))
(define-empty-tokens lam-toks
  (lam plus minus true false lpar rpar def end))

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
         [":="     (token-def)]
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
        null
        (cons result (lex input)))))

;; parse : (-> token?) -> (listof exp?)
;; parse the input and return an AST
(define parse
  (parser 
   (grammar (program
             [(top-level)
              (cons $1 null)]
             [(top-level program)
              (cons $1 $2)])
            (top-level
             [(exp) $1]
             [(definition) $1])
            (exp
             [(lam-exp) $1]
             [(arith-exp) $1]
             [(var) (make-var-exp $1)]
             [(nat) (make-num-exp $1)]
             [(bool-exp) $1]
             [(appl-exp) $1])
            (definition
              [(var def exp)
               (make-def $1 $3)])
            (lam-exp
             [(lpar lam var exp rpar)
              (make-lam-exp $3 $4)])
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
   (start program)
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
  (check-equal? (lex-once (open-input-string "true false")) (token-true))
  (check-equal? (lex-once (open-input-string ":=")) (token-def)))

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
                  (list (make-num-exp 5))))
   (test-case
    "Lambda expression"
    (check-equal? (test-parse "(lambda x (1 + x))")
                  (list lam-ex)))
   (test-case
    "Application"
    (check-equal? (test-parse "((lambda x (1 + x)) 5)")
                  (list (make-appl-exp lam-ex
                                       (make-num-exp 5)))))))

(define run-parser-tests
  (lambda () (test/gui simple-lexing-tests 
                       full-lexing-tests
                       parsing-tests)))