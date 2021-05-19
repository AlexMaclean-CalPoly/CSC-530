#lang rosette

(require "imp.rkt" "lift.rkt" "ivl.rkt")

(provide verify-wp verify-sp check-se)

; Takes as input an IVL statement and a postcondition
; Q expressed as a Rosette boolean? value, and returns
; the weakest (liberal) precondition of S with respect
; to Q, expressed as a Rosette boolean? value.
(define (wp S Q)
  (match S
    [`(skip) Q]
    [`(abort) #t]
    [`(begin) Q]
    [`(begin ,S1 ,S2 ...) (wp S1 (wp `(begin ,@S2) Q))]
    [`(:= ,x ,E) (substitute Q (int x) (int E))]
    [`(if ,(app bool C) ,S1 ,S2) (&& (=> C (wp S1 Q)) (=> (! C) (wp S2 Q)))]
    [`(assume ,C) (=> (bool C) Q)]
    [`(assert ,C) (&& (bool C) Q)]
    [`(havoc ,x)  (define-symbolic* h integer?)
                  (substitute Q (int x) h)]))

; Takes as input an IMP program annotated with pre/post
; conditions and loop invariants, translates it to IVL 
; by cutting loops, and uses WP to verify the resulting
; IVL program.  
(define (verify-wp prog)
  (match (cut (imp-ast prog))
    [`(procedure ,_ ... ,S)
     (verify (assert (wp S #t)))]))

; Define the semantics for the assume, assert, and havoc statements.
(interpretS+
 (lambda (S env)
   (match S
     [`(assume ,C) (and (interpretC C env) env)]
     [`(assert ,C) (assert (interpretC C env)) env]
     [`(havoc ,x)  (define-symbolic* h integer?)
                   (env-set env x h)])))

; Takes as input an IMP program annotated with pre/post
; conditions and loop invariants, translates it to IVL 
; by cutting loops, and uses SP to verify the resulting
; IVL program.  
(define (verify-sp prog)
  (match (cut (imp-ast prog))
    [(and `(procedure ,_ ,args ,_ ...) ivl)
     (verify (interpret ivl (map int args)))]))

; Takes as input an IMP program annotated with pre/post
; conditions, unrolls all loops k times, and uses SE to check 
; that all executions with up to k iterations of each loop
; are correct.  
(define (check-se prog k)
  (match (unroll (imp-ast prog) k)
    [(and `(procedure ,_ ,args ,_ ...) ivl)
     (verify (interpret ivl (map int args)))]))