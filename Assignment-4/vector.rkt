#lang typed/racket

(provide vect? vect-i*int vect-i+ vect-subst to-vect-x negate-vect vect-x+ vect-subst-const)

(require "types.rkt")

(define (vect-x+ [a : VectX] [b : VectX]) : VectX
  (VectX (terms-x+ (VectX-terms a) (VectX-terms b))))

(define (vect-i+ [a : VectI] [b : VectI]) : VectI
  (VectI (terms-i+ (VectI-terms a) (VectI-terms b))))

(: vect? (Any -> Boolean : Vect))
(define (vect? a)
  (or (VectX? a) (VectI? a)))

(define (vect-i*int [v : VectI] [i : Integer]) : VectI
  (VectI (terms-i*int (VectI-terms v) i)))

(define (negate-vect [v : Vect]) : Vect
  (match v
    [(VectX terms) (VectX (terms-x+ (terms-x*int terms -1) #hash((1 . #hash((1 . -1))))))]
    [(VectI terms) (VectI (terms-i+ (terms-i*int terms -1) #hash((1 . -1))))]))

(define (to-vect-x [v : Vect]) : VectX
  (match v
    [(? VectX?) v]
    [(VectI terms) (VectX (terms-i->x terms))]))

(define (vect-subst [what : VectI] [for : Symbol] [in : VectX]) : VectX
  (VectX (terms-subst (VectI-terms what) for (VectX-terms in))))

(define (vect-subst-const [what : Integer] [for : Symbol] [in : VectX]) : VectX
  (VectX (terms-subst-const what for (VectX-terms in))))

;; ---------------------------------------------------------------------------------------------------

(define (terms-i*int [v : TermsI] [i : Integer]) : TermsI
  (for/hash : TermsI ([([var : Variable] [coef : Integer]) v])
    (values var (* i coef))))

(define (terms-x*int [v : TermsX] [i : Integer]) : TermsX
  (for/hash : TermsX ([([var : Variable] [coef : TermsI]) v])
    (values var (terms-i*int coef i))))

(define (terms-i->x [t : TermsI]) : TermsX
  (for/hash : TermsX ([([var : Variable] [coef : Integer]) t])
    (values var (make-immutable-hash (list (cons 1 coef))))))

(define (terms-i*constant [v : TermsI] [c : TermsI]) : TermsX
  (for/hash : TermsX ([([var : Variable] [coef : Integer]) v])
    (values var (terms-i*int c coef))))

(define (terms-subst [what : TermsI] [for : Symbol] [in : TermsX]) : TermsX
  (if (hash-has-key? in for)
      (terms-x+ (terms-i*constant what (hash-ref in for)) (hash-remove in for)) in))

(define (terms-subst-const [what : Integer] [for : Symbol] [in : TermsX]) : TermsX
  (for/hash : TermsX ([([var : Variable] [coef : TermsI]) in])
    (define v (first (hash-keys coef)))
    (values var (if (and (symbol? v) (equal? (symbol->string for) (symbol->string v)))
                    (make-immutable-hash (list (cons 1 what)))
                    coef))))

(module wrapper racket/base
  (provide (all-defined-out))
  (require racket/hash)

  (define (terms-x+ a b)
    (hash-union a b #:combine terms-i+))

  (define (terms-i+ a b)
    (hash-union a b #:combine +)))

(require/typed 'wrapper
               [terms-x+ (TermsX TermsX -> TermsX)]
               [terms-i+ (TermsI TermsI -> TermsI)])

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)
  (check-equal? (vect-i+ (VectI #hash((x . 10) (y . 15))) (VectI #hash((x . 12) (z . 8))))
                (VectI #hash((x . 22) (y . 15) (z . 8))))
  (check-equal? (vect-i*int (VectI #hash((x . 10) (y . 1))) 2)
                (VectI #hash((x . 20) (y . 2))))
  (check-equal?
   (vect-subst (VectI #hash((x . 3) (y . 4))) 'x
               (VectX #hash((x . #hash((a1 . 1) (a2 . 4))) (y . #hash((a3 . 1) (a4 . 2))))))
   (VectX #hash((x . #hash((a1 . 3) (a2 . 12)))
                (y . #hash((a1 . 4) (a2 . 16) (a3 . 1) (a4 . 2))))))
  (check-equal? (negate-vect (VectI #hash((1 . 5) (x . 4)))) (VectI #hash((1 . -6) (x . -4)))))