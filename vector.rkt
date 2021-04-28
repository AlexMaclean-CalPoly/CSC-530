#lang typed/racket/no-check

(provide (all-defined-out))

(require "types.rkt")

(require/typed racket/hash
               [hash-union (Vect Vect [#:combine (Constant Constant -> Constant)] -> Vect)])

(define (vect-x+ [a : Vect-x] [b : Vect-x]) : Vect-x
  (hash-union a b #:combine vect-i+))

(define (vect-i+ [a : Vect-i] [b : Vect-i]) : Vect-i
  (hash-union a b #:combine +))

(define (vect+int [a : Vect-i] [i : Integer]) : Vect-i
  (hash-set a 1 (+ i (hash-ref a 1 (thunk 0)))))
    

(define (vect-i*int [v : Vect-i] [i : Integer]) : Vect-i
  (make-immutable-hash (hash-map v (lambda ([var : Symbol] [coef : Integer]) (cons var (* i coef))))))

(define (vect-i*constant [v : Vect-i] [c : Vect-i]) : Vect-x
  (make-immutable-hash (hash-map v (lambda ([var : Symbol] [coef : Integer]) (cons var (vect-i*int c coef))))))

(define (vect-subst [what : Vect-i] [for : Symbol] [in : Vect-x]) : Vect-x
  (if (hash-has-key? in for) (vect-x+ (vect-i*constant what (hash-ref in for)) (hash-remove in for)) in))


(module+ test
  (require typed/rackunit)
  (check-equal? (vect-i+ #hash((x . 10) (y . 15)) #hash((x . 12) (z . 8)))
                #hash((x . 22) (y . 15) (z . 8)))
  (check-equal? (vect-i*int #hash((x . 10) (y . 1)) 2)
                #hash((x . 20) (y . 2)))
  (check-equal? (vect-subst #hash((x . 3) (y . 4)) 'x #hash((x . #hash((a1 . 1) (a2 . 4))) (y . #hash((a3 . 1) (a4 . 2)))))
                #hash()))