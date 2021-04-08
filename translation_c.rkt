#lang typed/racket

(require "parse.rkt")

(define-type ExprC (U ExprLC FunC EnvC))
(struct FunC ([name : Symbol]) #:transparent)
(struct EnvC ([i : Integer]) #:transparent)

(define-type ProgramC (Mutable-HashTable Symbol ExprC))



(define (top-translate [s : Sexp]) : Any
  (define funs : ProgramC (make-hash))
  (define main (extract-funs (parse s) funs '()))
  (string-join (hash-map funs c-translate-fun) "\n\n"))

(define (extract-funs [e : ExprLC] [funs : ProgramC]) : ExprC
  (match e
    [(? real?) e]
    [(? symbol?) e]
    [(PrimC name args) (PrimC name (map (λ ([arg : ExprLC]) (extract-funs arg funs env)) args))]
    [(AppC f arg) (AppC (extract-funs f funs env) (extract-funs arg funs env))]
    [(LamC arg body) (add-fun body funs env)]))

(define (uncover-env [e : ExprLC] [env: (Listof Symbol)]) : ExprLC
  (match e
    [(? real?) e]
    [(? symbol?) (index

(define (add-fun [e : ExprC] [funs : ProgramC]) : FunC
  (define name (gensym 'lambda))
  (hash-set! funs name (extract-funs e funs env))
  (FunC name))

(define (c-translate [e : ExprC]) : String
  (match e
    [(? real?) (~v e)]
    [(? symbol?) (~a e)]
    [(PrimC 'println (list arg)) (format "printf(\"%d\\n\", (int) ~a)" (c-translate arg))]
    [(PrimC 'ifleq0 (list f t e))
     (format "((int) ~a <= 0 ? ~a : ~a)" (c-translate f) (c-translate t) (c-translate e))]
    [(PrimC '+ (list a b)) (format "((int) ~a + (int) ~a)" a b)]
    [(PrimC '* (list a b)) (format "((int) ~a * (int) ~a)" a b)]
    [(AppC name arg) ] ))

(define (c-translate-fun [s : Symbol] [e : ExprC]) : String
  (format "void* ~a(void* env) {
    return ~a;
}" s (c-translate e)))

(displayln (top-translate 5))
(displayln (top-translate '(lam x (+ x 1))))
(displayln (top-translate '(lam x (+ x ((lam y (ifleq0 y 1 -1)) 0)))))

