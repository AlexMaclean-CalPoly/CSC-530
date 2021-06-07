#lang typed/racket

(provide top-c)

(require "parse.rkt")

(define-type ExprC (U Real FunC EnvC PrimC AppC))
(struct FunC ([name : Symbol]) #:transparent)
(struct EnvC ([i : (U Symbol Integer)] [name : Symbol]) #:transparent)
(struct AppC ([f : ExprC] [arg : ExprC]) #:transparent)
(struct PrimC ([name : Symbol] [args : (Listof ExprC)]) #:transparent)

(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC] [len : Integer]) #:transparent)

(define-type ProgramC (Mutable-HashTable Symbol FunDefC))
(define-type Env (Listof Symbol))

;; Given an S expression returns a string representation in C
(define (top-c [s : Sexp]) : String
  (define funs : ProgramC (make-hash))
  (define main (extract-funs (parse s) funs '()))
  (string-append header
                 (string-join (map c/FunDefC (reverse (sort (hash-values funs) FunDefC<?))))
                 (c/main main)))

;; Transforms and AST into a abstract syntax representation of a C program
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

;; Given an ExprC returns a C string
(define (c/ExprC [e : ExprC] [env : String]) : String
  (match e
    [(? real?) (~v e)]
    [(EnvC (? symbol? id) _) (~a id)]
    [(EnvC i id) (format "env[~a /* ~a */]" i id)]
    [(PrimC 'println (list arg)) (format "printf(\"%d\\n\", (long) ~a)" (c/ExprC arg env))]
    [(PrimC 'ifleq0 (list f t e))
     (format "((long) ~a <= 0 ? ~a : ~a)" (c/ExprC f env) (c/ExprC t env) (c/ExprC e env))]
    [(PrimC '+ (list a b)) (format "((long) ~a + (long) ~a)" (c/ExprC a env) (c/ExprC b env))]
    [(PrimC '* (list a b)) (format "((long) ~a * (long) ~a)" (c/ExprC a env) (c/ExprC b env))]
    [(AppC f arg) (format "app(~a, ~a)" (c/ExprC f env) (c/ExprC arg env))]
    [(FunC name) (format "lam(&~a, ~a)" name env)]))

;; Given a FunDefC returns a C string
(define (c/FunDefC [f : FunDefC]) : String
  (match f
    [(FunDefC name arg body len)
        (format func name arg (c/ExprC body (format "ext(~a, env, ~a)" arg len)))]))

;; Given a ExprC representing the main function returns a C string
(define (c/main [e : ExprC]) : String
  (format main (c/ExprC e "NULL")))

;; Compares 2 FunDefCs for sorting based on definition dependency
(define (FunDefC<? [a : FunDefC] [b : FunDefC]) : Boolean
  (< (FunDefC-len a) (FunDefC-len b)))

;; Templates -----------------------------------------------------------------------------------------

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
