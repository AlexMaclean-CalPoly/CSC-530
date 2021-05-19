#lang rosette

(require "imp.rkt" "tools.rkt")

; This is how we construct IMP programs that 
; are annotated with requires / ensures clauses
; and loop invariants.
(procedure succ (x) : (y)
  (#:requires true)
  (#:ensures (= y (+ x 1)))
  (:= y (+ x 1)))

(procedure same (x) : (y)
  (#:requires (>= x 0))
  (#:ensures (= y x))   
  (begin
    (:= y 0)
    (while (< y x)
      (#:invariant (<= y x))
      (:= y (+ y 1)))))

; All three verifiers agree that both of these
; programs are correct. Note that bounded verification
; is sufficient for succ since it has no loops.
(verify-wp succ)
(verify-sp succ)
(check-se succ 0)

(verify-wp same)
(verify-sp same)
(check-se same 0)
(check-se same 1)
(check-se same 2)

; If we provide the wrong loop invariant for same,
; verification fails, but we can't find any bugs with SE
; because the program is correct.
(procedure same-weak (x) : (y)
  (#:requires (>= x 0))
  (#:ensures (= y x))   
  (begin
    (:= y 0)
    (while (< y x)
      (#:invariant true)
      (:= y (+ y 1)))))

(verify-wp same-weak)
(verify-sp same-weak)
(check-se same-weak 0)
(check-se same-weak 1)
(check-se same-weak 2)

; But if the program is buggy, then SE will find a real counterexample.
(procedure same-buggy (x) : (y)
  (#:requires (>= x 0))
  (#:ensures (= y x))   
  (begin
    (:= y 0)
    (while (< y x)
      (#:invariant true)
      (:= y (+ y 2)))))

(verify-wp same-buggy)
(verify-sp same-buggy)
(check-se same-buggy 0)
(check-se same-buggy 1)
(check-se same-buggy 2)