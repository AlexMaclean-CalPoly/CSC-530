#lang rosette

(provide cut unroll)

; Takes as input the AST of an IMP program that is annotated
; with requires / ensures clauses and loop invariants, and
; returns the corresponding program in the IMP intermediate
; verification language (IVL). IVL programs contain no annotations
; and no loops, but may include havoc, assert, and assume statements.
; This procedure transforms annotated IMP programs to IVL by cutting
; loops, as shown in lecture. The procedure also transforms the 
; requires / ensures clauses into suitable IVL statements
; in the body of the generated program.
(define (cut ast)
  (match ast
    [`(procedure ,id ,arg : ,ret (#:requires ,P) (#:ensures ,Q) ,S)
     (error 'cut "not yet implemented")]
    [`(while ,C (#:invariant ,I) ,S) (error 'cut "not yet implemented")]
    ; handle the remaining IMP statements ...
    ))

; Takes as input the AST of an IMP program that is annotated
; with requires / ensures clauses, and returns a finitized
; version of this program in IVL that unrolls all loops k >= 0 times.
; Specifically, the resulting IVL program can perform up to
; k iterations of every loop in the original IMP program, ensuring
; that there are no inputs on which the transformed program fails and
; the original one does not. The procedure also transforms the 
; requires / ensures clauses into suitable IVL statements
; in the body of the generated program.
(define (unroll ast k)
  (match ast
    [`(procedure ,id ,arg : ,ret (#:requires ,P) (#:ensures ,Q) ,S)
     (error 'unroll "not yet implemented")]
    [`(while ,C (#:invariant ,I) ,S) (error 'unroll "not yet implemented")]
    ; handle the remaining IMP statements ...
    ))