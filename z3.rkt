#lang typed/racket/no-check


(require "vector.rkt" "logic.rkt" "types.rkt")
(provide to-z3)

(define (to-z3 [l : (Listof assert-=)]) : (Listof Sexp)
  `(,@(map (lambda ([s : Symbol]) `(declare-const ,s Int)) (append (get-lambdas l) (get-lambda-n l) (get-unknowns l)))
    ,@(map to-z3/assert l)
    ,@(map (lambda ([s : Symbol]) `(assert (>= ,s 0))) (get-lambda-n l))
    ,@(map (lambda ([s : Symbol]) `(assert (> ,s 0))) (get-lambdas l))
    (check-sat)
    (get-model)))


(define (to-z3/assert [a : assert-=]) : Sexp
  `(assert (= ,(vect->sexp (assert-=-v a)) ,(match (assert-=-o a) [0 0] [(? symbol? s) `(* -1 ,s)]))))

(define (get-lambdas [l : (Listof assert-=)]) : (Listof Symbol)
  (remove-duplicates (filter symbol? (map assert-=-o l))))

(define (get-lambda-n [l : (listof assrt-=)]) : (Listof Symbol)
  (remove-duplicates (filter symbol? (append-map (lambda ([x : assert-=]) (hash-keys (assert-=-v x))) l))))

(define (get-unknowns [l : (listof assrt-=)]) : (Listof Symbol)
  (remove-duplicates (filter symbol? (append-map (lambda ([x : assert-=]) (append-map hash-keys (hash-values (assert-=-v x)))) l))))

;(define (get-vars [l : (Listof assert-=)]) : (Listof Symbols)
;  (append-map (lambda ([a : assert-=]) (cons (assert-=-o a) (get-vars/Vect-x (assert-=-v a)))) l))