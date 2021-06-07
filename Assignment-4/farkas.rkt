#lang typed/racket

(provide apply-farkas)

(require "types.rkt" "vector.rkt" "logic.rkt")

(define (apply-farkas [clauses : (Listof Logic)]) : (Listof Assert=)
  (append-map (λ ([l : DisjunctionL]) (clause-f (demorgan l)))                                        
              (cast (append-map ConjunctionL-clauses (cast clauses (Listof ConjunctionL)))
                    (Listof DisjunctionL))))

(define (clause-f [l : (Listof VectX)]) : (Listof Assert=)
  (define sum : (Mutable-HashTable Variable VectX) (make-hash))
  (for ([e : VectX l])
    (define lam (gensym 'lambda))
    (for ([([var : Variable] [val : TermsI]) (VectX-terms e)])
      (hash-set! sum var
                 (vect-x+ (hash-ref sum var (thunk (VectX #hash((1 . #hash((1 . 0)))))))
                          (VectX (make-immutable-hash (list (cons lam val))))))))
  (hash-map sum (λ ([var : Variable] [val : VectX])
                (Assert= val (if (equal? var 1) (gensym 'lambda*) 0)))))

(define (demorgan [l : DisjunctionL]) : (Listof VectX)
  (cast (map negate-vect (cast (DisjunctionL-clauses l) (Listof VectX))) (Listof VectX)))
