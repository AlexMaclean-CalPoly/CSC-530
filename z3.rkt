#lang typed/racket

(provide to-z3)

(require "vector.rkt" "logic.rkt" "types.rkt")

(define (to-z3 [l : (Listof Assert=)]) : (Listof Sexp)
  `(,@(map (λ ([s : Symbol]) `(declare-const ,s Int)) (get-all-vars l))
    ,@(map to-z3/assert l)
    ,@(map (λ ([s : Symbol]) `(assert (>= ,s 0))) (get-lambda-n l))
    ,@(map (λ ([s : Symbol]) `(assert (> ,s 0))) (get-lambdas l))
    (check-sat)
    (get-model)))

(define (to-z3/assert [a : Assert=]) : Sexp
  `(assert (= ,(vect->sexp (Assert=-v a)) ,(match (Assert=-o a) [0 0] [(? symbol? s) `(* -1 ,s)]))))

(define (get-all-vars [l : (Listof Assert=)]) : (Listof Symbol)
  (append (get-lambdas l) (get-lambda-n l) (get-unknowns l)))

(define (get-lambdas [l : (Listof Assert=)]) : (Listof Symbol)
  (remove-duplicates (filter symbol? (map Assert=-o l))))

(define (get-lambda-n [l : (Listof Assert=)]) : (Listof Symbol)
  (remove-duplicates (filter symbol? (append-map (λ ([x : Assert=])
                                                   (hash-keys (VectX-terms (Assert=-v x)))) l))))

(define (get-unknowns [l : (Listof Assert=)]) : (Listof Symbol)
  (remove-duplicates
   (filter symbol? (append-map (λ ([x : Assert=])
                                 (append-map hash-keys
                                             (hash-values (VectX-terms (Assert=-v x))))) l))))
