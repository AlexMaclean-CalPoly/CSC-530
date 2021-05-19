#lang rosette

(require "imp.rkt")
(provide (all-defined-out))

; This is how we construct an IMP programs that 
; returns the successor of a given integer.
(procedure succ (x) : (y)
  (:= y (+ x 1)))

; The above declaration constructs an imp? object, 
; which contains the AST of the succ procedure, as 
; well as a Rosette procedure that inteprets the 
; succ AST on a given input.
(imp? succ)
(pretty-print succ)
(for ([x '(-100 0 100)])
  (displayln `(succ (,x) : ,(succ x))))

; Here is an IMP program that uses a loop to implement 
; the identity function on non-negative integers.
(procedure same (x) : (y)
  (begin
    (:= y 0)
    (while (< y x)
      (:= y (+ y 1)))))

(newline)
(pretty-print same)
(for ([x 4])
  (displayln `(same (,x) : ,(same x))))

; IMP programs can contain conditionals, like this 
; program that computes the absolute value of its input.
(procedure absolute (x) : (y)
  (begin
    (:= y x)
    (if (< x 0)
        (:= y (- x))
        (skip))))

(newline)
(pretty-print absolute)
(for ([x '(-42 0 42)])
  (displayln `(absolute (,x) : ,(absolute x))))


; IMP programs can return multiple values, like this 
; program that computes the quotient and remainder 
; obtained by diving a non-negative integer n with a
; positive integer d. 
(procedure div (n d) : (q r)
  (begin
    (:= q 0)
    (:= r n)
    (while (>= r d)
      (begin
        (:= q (+ q 1))
        (:= r (- r d))))))

(newline)
(pretty-print div)
(for ([n '(0   1 4 20)]
      [d '(100 3 2 6)])
  (displayln `(div (,n ,d) : ,(div n d))))

; IMP programs can also contain nested loops and abort
; statements, like this program that decides if its input
; is part of a Pythagorean triple. The program takes as
; input a positive integer c and either aborts or returns
; positive integers a and b such that c^2 = a^2 + b^2.
(procedure pythagorean (c) : (a b)
  (begin
    (:= a 1)
    (:= b 1)
    (while (< (+ (* a a) (* b b)) (* c c))  
      (begin
        (while (< (+ (* a a) (* b b)) (* c c)) 
          (:= b (+ b 1)))
        (if (> (+ (* a a) (* b b)) (* c c))
          (begin
            (:= a (+ a 1))
            (:= b 1))
          (skip))))
    (if (= (+ (* a a) (* b b)) (* c c))
      (skip)
      (abort))))

(newline)
(pretty-print pythagorean)
(for ([c (in-range 1 21)])
  (displayln `(pythagorean (,c) : ,(pythagorean c))))