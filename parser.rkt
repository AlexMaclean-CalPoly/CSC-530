#lang typed/racket

(provide parse)

(require "types.rkt")

#|

Program = (Stmt ...)

Stmt = (id := Vector)
     | (id := ?)
     | (while Pred (Stmt ...))
     | (assert Pred)
     | (assume Pred)
     | (if Pred (Stmt ...))

Ve   = id
     | Num
     | (Num id)

Vector = (Ve ...)

Pred = (Pred && Pred)
     | (Pred || Pred)
     | (! Pred)
     | (Vector >= 0)

|#

(define (parse [s : Sexp]) : Program
  (parse/Program s))

(define (parse/Program [s : Sexp]) : Program
  (if (list? s) (map parse/Stmt s) (error 'parse "Invalid Program ~e" s)))

(define (parse/Stmt [s : Sexp]) : Stmt
  (match s
    [`(,(? symbol? id) := ?) (Assign id (make-immutable-hash (list (cons (gensym 'r) 1))))]
    [`(,(? symbol? id) := ,exp) (Assign id (parse/Vect exp))]
    [`(while ,test ,(? list? body)) (While (parse/Logic test) (parse/Program body))]
    [`(if ,test ,(? list? body)) (If (parse/Logic test) (parse/Program body))]
    [`(assert ,e) (Assert (parse/Logic e))]
    [`(assume ,e) (Assume (parse/Logic e))]
    [_ (error 'parse "Invalid Stmt ~e" s)]))

(define (parse/Logic [s : Sexp]) : Logic
  (match s
    [`(,a && ,b) (ConjunctionL (list (parse/Logic a) (parse/Logic b)))]
    [`(,a || ,b) (DisjunctionL (list (parse/Logic a) (parse/Logic b)))]
    [`(! ,a) (NotL (parse/Logic a))]
    [`(,v >= 0) (parse/Vect v)]
    [_ (error 'parse "Invalid Logic ~e" s)]))

(define (parse/Vect [s : Sexp]) : Vect-i
  (unless (list? s)
    (error 'parse "Invalid Vect ~e" s))
  (make-immutable-hash (map parse/Ve s)))

(define (parse/Ve [s : Sexp]) : (Pairof (U One Symbol) Integer)
  (match s
    [`(,(? exact-integer? a) ,(? symbol? b)) (cons b a)]
    [(? exact-integer?) (cons 1 s)]
    [(? symbol?) (cons s 1)]
    [_ (error 'parse "Invalid Vector Entry ~e" s)]))


;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse/Program
                 '{{x := (-50)}
                   {while (! ((x) >= 0))
                          {
                           {x := (x y)}
                           {y := (y 1)}
                           }}
                   {assert((y -1) >= 0)}})
                (list)))