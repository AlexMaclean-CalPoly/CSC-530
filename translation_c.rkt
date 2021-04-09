#lang typed/racket/no-check

(require "parse.rkt")

(define-type ExprC (U FunC EnvC PrimC Real Symbol AppC LamC))
(struct FunC ([name : Symbol] [len : Integer]) #:transparent)
(struct EnvC ([i : Integer] [name : Symbol]) #:transparent)

(struct LamC ([arg : Symbol] [body : ExprC] [len : Integer]) #:transparent)
(struct AppC ([f : ExprC] [arg : ExprC]) #:transparent)
(struct PrimC ([name : Symbol] [args : (Listof ExprC)]) #:transparent)

(define-type ProgramC (Mutable-HashTable Symbol ExprC))
(define-type Env (Listof Symbol))


(define (top-translate [s : Sexp]) : Any
  (define funs : ProgramC (make-hash))
  (define main (extract-funs (uncover-env (parse s) '()) funs))
  (string-join (append (hash-map funs c-translate-fun) (list (c-translate-main main))) "\n\n"))

(define (extract-funs [e : ExprLC] [funs : ProgramC]) : ExprC
  (match e
    [(? EnvC?) e]
    [(? real?) e]
    [(? symbol?) e]
    [(PrimC name args) (PrimC name (map (λ ([arg : ExprLC]) (extract-funs arg funs)) args))]
    [(AppC f arg) (AppC (extract-funs f funs) (extract-funs arg funs))]
    [(LamC arg body len) (add-fun body funs len)]))

(define (uncover-env [e : ExprLC] [env : Env]) : ExprC
  (match e
    [(? real?) e]
    [(? symbol?) (EnvC (index-of env e) e)]
    [(PrimLC name args) (PrimC name (map (lambda ([arg : ExprLC]) (uncover-env arg env)) args))]
    [(AppLC f arg) (AppC (uncover-env f env) (uncover-env arg env))]
    [(LamLC arg body) (LamC arg (uncover-env body (cons arg env)) (length env))]))


(define (add-fun [e : ExprC] [funs : ProgramC] [len : Integer]) : FunC
  (define name (gensym 'lambda))
  (hash-set! funs name (extract-funs e funs))
  (FunC name len))

(define (c-translate [e : ExprC]) : String
  (match e
    [(? real?) (~v e)]
    [(EnvC i id) (format "env[~a /* ~a */]" i id)]
    [(PrimC 'println (list arg)) (format "printf(\"%d\\n\", (int) ~a)" (c-translate arg))]
    [(PrimC 'ifleq0 (list f t e))
     (format "((int) ~a <= 0 ? ~a : ~a)" (c-translate f) (c-translate t) (c-translate e))]
    [(PrimC '+ (list a b)) (format "((int) ~a + (int) ~a)" (c-translate a) (c-translate b))]
    [(PrimC '* (list a b)) (format "((int) ~a * (int) ~a)" (c-translate a) (c-translate b))]
    [(AppC f arg) (format "run_closure(~a, ~a)" (c-translate f) (c-translate arg))]
    [(FunC name len) (format "create_closure(&~a, env, ~a)" name len)]))

(define (c-translate-fun [s : Symbol] [e : ExprC]) : String
  (format "void* ~a(void** env) {
    return ~a;
}" s (c-translate e)))

(define (c-translate-main [e : ExprC]) : String
  (format "int main() {
    return ~a;
}" (c-translate e)))


(displayln (top-translate 5))
(displayln (top-translate '(lam x (+ x 1))))
(displayln (top-translate '(lam x (+ x ((lam y (ifleq0 y 1 -1)) 0)))))
(displayln
    (top-translate
    '(println ((lam fact ((fact fact) 10))
                (lam self
                    (lam v
                        (ifleq0 v 1 (* v ((self self) (+ v -1))))))))))
