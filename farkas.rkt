#lang typed/racket/no-check

(require "types.rkt" "vector.rkt")
(provide apply-farkas)

  
(define (apply-farkas [clauses : (Listof Logic)]) : Any
  (append-map (lambda ([l : Logic]) (append-map clause-f (map demorgan (ConjunctionL-clauses l)))) clauses))

(define (clause-f [l : (Listof Vect)]) : (Listof assert-=)
  (define h : (Mutable-HashTable Variable Vect-x)  (make-hash))
  (for ([e l])
    (define lam (gensym 'lambda))
    (for ([(var val) e])
      (hash-set! h var
                 (vect-x+ (hash-ref h var (thunk #hash((1 . #hash((1 . 0))))))
                          (make-immutable-hash (list (cons lam val)))))))
  (hash-map h (lambda ([var : Variable] [val : Vect-x]) (assert-= val (if (equal? var 1) 0 (gensym 'lambda*))))))


(define (demorgan [l : Logic]) : (Listof Vect)
  (map negate-vect (DisjunctionL-clauses l)))