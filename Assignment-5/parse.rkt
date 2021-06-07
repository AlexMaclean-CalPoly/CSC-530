#lang racket

(provide parse-program parse-error-model)

(require "types.rkt")

(define (op? e)
  (if (member e '(+ - * / ** > == and or)) #t #f))

(define (parse-program sexp)
  (if (list? sexp) (to-stmt (map parse sexp)) (error "Bad Parse Program: " sexp)))

(define (to-stmt lst)
  (match lst
    [(list stmt) stmt]
    [(cons stmt rst) (cons stmt (to-stmt rst))]))

(define (parse sexp [rule? #f])
  (match sexp
    [(? op?) (Op sexp)]
    [(or (? integer?) (? symbol?)) sexp]
    [`(,(? symbol? var) = ,val) (Assign var (parse val rule?))]
    [`(if ,f : ,t else: ,e) (If (parse f rule?) (parse t rule?) (parse e rule?))]
    [`(return ,e) (Return (parse e rule?))]
    [`(not ,e) (Not (parse e rule?))]
    [`($ ,(? symbol? id)) #:when rule? (Subst id)]
    [`(? . ,choices) #:when rule? (Choices (map (λ (c) (parse c rule?)) choices))]
    [`(,a ,op ,b) (BinOp (parse a rule?) (parse op rule?) (parse b rule?))]))

(define (parse-error-model sexp)
  (if (list? sexp) (map parse-rule sexp) (error "Bad Error Model: " sexp)))

(define (parse-rule sexp)
  (match sexp
    [`(,before -> ,after) (Rewrite-Rule (parse before #t) (parse after #t))]))