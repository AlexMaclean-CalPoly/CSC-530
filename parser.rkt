#lang typed/racket/no-check

(provide parse)

(require "types.rkt" "vector.rkt")

#|

Program = (Stmt ...)

Stmt = (id := Vector)
     | (id := ?)
     | (while Pred (Stmt ...))
     | (assert Pred)
     | (assume Pred)
     | (if Pred (Stmt ...))

Vector = id
       | num
       | (Vector + Vector)
       | (Vector * num)
       | (num * Vector)

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
    [`(,v < 0) (NotL (parse/Vect v))]
    [_ (error 'parse "Invalid Logic ~e" s)]))

(define (parse/Vect [s : Sexp]) : Vect-i
  (match s
    [(? exact-integer?) (make-immutable-hash (list (cons 1 s)))]
    [(? symbol?) (make-immutable-hash (list (cons s 1)))]
    [`(,a + ,b) (vect-i+ (parse/Vect a) (parse/Vect b))]
    [`(,v * ,(? exact-integer? i)) (vect-i*int (parse/Vect v) i)]
    [`(,(? exact-integer? i) * ,v) (vect-i*int (parse/Vect v) i)]
    [_ (error 'parse "Invalid Vector ~e" s)]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse
                 '{{x := -50}
                   {while (x < 0)
                          {
                           {x := (x + y)}
                           {y := (y + 1)}
                           }}
                   {assert((y + -1) >= 0)}})
                (list
                 (Assign 'x '#hash((1 . -50)))
                 (While
                  (NotL '#hash((x . 1)))
                  (list (Assign 'x #hash((x . 1) (y . 1)))
                        (Assign 'y #hash((1 . 1) (y . 1)))))
                 (Assert '#hash((1 . -1) (y . 1))))))