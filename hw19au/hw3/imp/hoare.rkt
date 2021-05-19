#lang rosette

(require "imp.rkt" "lift.rkt")

(provide verify-claims)

(define (proof-error expected given)
  (error 'verify-hoare "proof error\n  expected: ~a\n  given: ~a" expected given))

(define (verify-consequence P Q)
  (unless (unsat? (verify (assert (=> (bool P) (bool Q)))))
    (proof-error "P => Q" (list P Q))))

; Throws a proof error if the {P} S {Q} is not a valid Hoare triple.
; Otherwise terminates successfully (returning void).
(define (verify-triple P S Q)
  ;(displayln `(verify-triple ,P ,S ,Q)) ; Uncomment to follow the proof process.
  (match S
    [`(skip)  (verify-consequence P Q)]
    [`(abort) (void)]  
    [`(:= ,x ,E)
     (unless (eqv? (bool P) (substitute (bool Q) (int x) (int E)))
       (proof-error 'assignment `(,P (:= ,x ,E) ,Q)))]
    [`(if ,C ,S1 ,S2)
     (verify-triple `(&& ,P ,C) S1 Q)
     (verify-triple `(&& ,P (! ,C)) S2 Q)]
    [`(while ,C ,S1)
     (unless (eqv? (bool `(&& ,P (! ,C))) (bool Q))
       (proof-error "{P} (while C body) {P ∧ ¬C}" (list P S Q)))
     (verify-triple `(&& ,P ,C) S1 P)]
    [`(begin ,T ...)
     (let verify-begin ([P1 P][T1 T])
       (match T1
         [`() (verify-consequence P1 Q)]
         [`((#:claim ,P2) ,T2 ...)
          (verify-consequence P1 P2)
          (verify-begin P2 T2)]
         [`(,S1 (#:claim ,Q1) ,T1 ...)
          (verify-triple P1 S1 Q1)
          (verify-begin Q1 T1)]
         [`(,S1) (verify-triple P1 S1 Q)]))]))
  
; Takes as input an IMP procedure that is fully annotated with proof claims,
; and applies the inference rules for Hoare Logic to check that the claims 
; are correct; i.e., they constitute a valid proof in Hoare Logic.
(define (verify-claims prog)
  (match (imp-ast prog)
    [`(procedure ,_ ,_ : ,_ (begin (#:claim ,P) ,S ... (#:claim ,Q)))
     (verify-triple P `(begin ,@S) Q)]))
                                            


                          