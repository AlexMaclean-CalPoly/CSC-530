#lang typed/racket

(require "parse.rkt")

(define-type ExprC (U FunC EnvC PrimC Real Symbol AppC LamC))
(struct FunC ([name : Symbol]) #:transparent)
(struct EnvC ([i : (U Symbol Integer)] [name : Symbol]) #:transparent)

(struct LamC ([arg : Symbol] [body : ExprC] [len : Integer]) #:transparent)
(struct AppC ([f : ExprC] [arg : ExprC]) #:transparent)
(struct PrimC ([name : Symbol] [args : (Listof ExprC)]) #:transparent)

(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC] [len : Integer]) #:transparent)

(define-type ProgramC (Mutable-HashTable Symbol FunDefC))
(define-type Env (Listof Symbol))


(define (top-translate [s : Sexp]) : Any
  (define funs : ProgramC (make-hash))
  (define main (extract-funs (parse s) funs '()))
  (string-append header
                 (string-join (map c-translate-fun (reverse (sort (hash-values funs) FunDefC<?))))
                 (c-translate-main main)))

(define (extract-funs [e : ExprLC] [funs : ProgramC] [env : Env]) : ExprC
  (match e
    [(? real?) e]
    [(? symbol?) (EnvC (or (index-of (rest env) e) e) e)]
    [(PrimLC name args) (PrimC name (map (λ ([arg : ExprLC]) (extract-funs arg funs env)) args))]
    [(AppLC f arg) (AppC (extract-funs f funs env) (extract-funs arg funs env))]
    [(LamLC arg body)
     (let [(name (gensym 'lambda))]
       (hash-set! funs name (FunDefC name arg (extract-funs body funs (cons arg env)) (length env)))
       (FunC name))]))


(define (c-translate [e : ExprC] [env : String]) : String
  (match e
    [(? real?) (~v e)]
    [(EnvC (? symbol? id) _) (~a id)]
    [(EnvC i id) (format "env[~a /* ~a */]" i id)]
    [(PrimC 'println (list arg)) (format "printf(\"%d\\n\", (long) ~a)" (c-translate arg env))]
    [(PrimC 'ifleq0 (list f t e))
     (format "((long) ~a <= 0 ? ~a : ~a)" (c-translate f env) (c-translate t env) (c-translate e env))]
    [(PrimC '+ (list a b)) (format "((long) ~a + (long) ~a)" (c-translate a env) (c-translate b env))]
    [(PrimC '* (list a b)) (format "((long) ~a * (long) ~a)" (c-translate a env) (c-translate b env))]
    [(AppC f arg) (format "app(~a, ~a)" (c-translate f env) (c-translate arg env))]
    [(FunC name) (format "lam(&~a, ~a)" name env)]))

(define (c-translate-fun [f : FunDefC]) : String
  (match f
    [(FunDefC name arg body len)
        (format func name arg (c-translate body (format "ext(~a, env, ~a)" arg len)))]))

(define (c-translate-main [e : ExprC]) : String
  (format main (c-translate e "NULL")))

(define (FunDefC<? [a : FunDefC] [b : FunDefC])
  (< (FunDefC-len a) (FunDefC-len b)))

(define main "
int main() {
    return ~a;
}
")

(define func "
void* ~a(void* ~a, void** env) {
    return ~a;
}
")

(define header "
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

typedef struct Closure {
    void* (*func)(void*, void**);
    void** env;
} Clo;

Clo* lam(void* (*func)(void*, void**), void** env) {
    Clo* clo = (Clo*)malloc(sizeof(Clo));
    clo->func = func;
    clo->env = env;
    return clo;
}

void** ext(void* arg, void** env, int len) {
    void** new_env = (void**)malloc(sizeof(void*)*len+1);
    new_env[0] = arg;
    memcpy(new_env+1, env, len);
    return new_env;
}

void* app(void* c, void* arg) {
    Clo* clo = (Clo *)c;
    return (*(clo->func))(arg, clo->env);
}
")



(displayln (top-translate 5))
(displayln (top-translate '(lam x (+ x 1))))
(displayln (top-translate '(lam x (+ x ((lam y (ifleq0 y 1 -1)) 0)))))
(displayln
    (top-translate
    '(println ((lam fact ((fact fact) 10))
                (lam self
                    (lam v
                        (ifleq0 v 1 (* v ((self self) (+ v -1))))))))))
