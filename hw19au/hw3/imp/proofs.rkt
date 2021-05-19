#lang rosette

(require "imp.rkt" "hoare.rkt")
(provide (all-defined-out))

; Here is a complete proof outline that proves the
; following Hoare triple for the succ procedure:
; {true} succ (x) : (y) {y = x + 1}
(procedure succ (x) : (y)
  (begin
    (#:claim true)  
    (#:claim (= (+ x 1) (+ x 1)))  ; Consequence
    (:= y (+ x 1))
    (#:claim (= y (+ x 1)))))      ; Assignment

; The IMP interpreter ignores the proof annotations at runtime.
; So you can run annotated IMP programs as in examples.rkt.
; The annotations are used only for verification.
(verify-claims succ)

; Note that the IMP verifier does a bit more than a plain
; syntactic check when applying the Rule of Assignment.
; This allows it to verify a slightly less verbose version
; of the proof outline for succ.
(procedure succ-alt (x) : (y)
  (begin
    (#:claim true)              
    (:= y (+ x 1))
    (#:claim (= y (+ x 1)))))     ; Assignment

(verify-claims succ)

; Here is a complete proof outline for the same procedure.
; The outline proves the following Hoare triple:
; { x >= 0 } same (x) : (y) { y = x }
(procedure same (x) : (y)
  (begin
    (#:claim (>= x 0))
    (#:claim (&& (>= x 0) (= 0 0)))     ; Consequence
    (:= y 0)
    (#:claim (&& (>= x 0) (= y 0)))     ; Assignment 
    (#:claim (<= y x))                  ; Consequence (loop invariant P)
    (while (< y x)                      ; C
      (begin
        (#:claim (&& (<= y x) (< y x))) ; P /\ C
        (#:claim (<= (+ y 1) x))        ; Consequence
        (:= y (+ y 1))
        (#:claim (<= y x))))            ; Assignment (P)
    (#:claim (&& (<= y x) (! (< y x)))) ; While (P /\ !C)
    (#:claim (= y x))))                 ; Consequence

(verify-claims same)

; A slightly less verbose outline is also accepted.
(procedure same-alt (x) : (y)
  (begin
    (#:claim (>= x 0))
    (:= y 0)
    (#:claim (&& (>= x 0) (= y 0)))     ; Assignment 
    (#:claim (<= y x))                  ; Consequence (loop invariant P)
    (while (< y x)                      ; C
      (begin
        (#:claim (<= (+ y 1) x))        ; Consequence of P /\ C
        (:= y (+ y 1))))                ; Assignment is just P
    (#:claim (&& (<= y x) (! (< y x)))) ; While (P /\ !C)
    (#:claim (= y x))))                 ; Consequence

(verify-claims same-alt)

; --------------------------------------------------------------------;
; Complete the following proof outlines to show that the given 
; Hoare triples are valid. When you are done, all the verify-claims
; calls will succeed. It is easier to first write the complete proof
; outline and then, if you want, clean it up to remove redundant
; claims. This cleanup step is optional; we'll give full credit to
; all solutions that are accepted by the verifier. 
; --------------------------------------------------------------------;

; { true } absolute (x) : (y) { (y >= 0) && (y = x || y = -x) }
; ~4 claims needed in addition to the given ones.
(procedure absolute (x) : (y)
  (begin 
    (#:claim true)
    (:= y x)
    (if (< x 0)
        (:= y (- x))
        (skip))
    (#:claim (&& (>= y 0) (|| (= y x) (= y (- x)))))))

(verify-claims absolute)

; { n >= 0 && d > 0 } div (n d) : (q r) { n = q*d+r && r >= 0 && r < d }
; ~6 claims needed in addition to the given ones.
(procedure div (n d) : (q r)
  (begin
    (#:claim (&& (>= n 0) (> d 0)))
    (:= q 0)
    (:= r n)
    (while (>= r d)
      (begin
        (:= q (+ q 1))
        (:= r (- r d))))
    (#:claim (&& (= n (+ (* q d) r)) (>= r 0) (< r d)))))

(verify-claims div)

; { c > 0 } pythagorean (c) : (a b) { a > 0 && b > 0 && c*c = a*a + b*b }
; ~9 claims needed in addition to the given ones.
(procedure pythagorean (c) : (a b)
  (begin
    (#:claim (> c 0))
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
        (abort))
    (#:claim (&& (> a 0) (> b 0) (= (* c c) (+ (* a a) (* b b)))))))

(verify-claims pythagorean)