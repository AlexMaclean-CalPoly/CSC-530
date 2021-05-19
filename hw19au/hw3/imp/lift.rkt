#lang rosette

(require "imp.rkt")
(provide bool int substitute)

; This module provides procedures for lifting IMP
; values into corresponding Rosette values.
; For example, (int '(+ b c)) produces the
; Rosette integer value (+ b c), where b and
; c are symbolic constants identified by the symbols
; 'b and 'c. As a result, the following always returns #t:
; > (eq? (int '(+ b c)) (int '(+ b c)))

; Defines a dictionary that maps every key it
; receives via dict-ref to a symbolic constant
; of type integer?, with key as its identifier.
(define symbolic-environment
  (let ()
    (struct symenv ()
      #:methods gen:dict
      [(define (dict-ref dict key [default #f])
         (constant key integer?))])
    (symenv)))

; Lifts an IMP conditional C into the
; corresponding Rosette boolean expression.
(define (bool C)
  (interpretC C symbolic-environment))

; Lifts an IMP expression E into the
; corresponding Rosette integer expression.
(define (int E)
  (interpretE E symbolic-environment))

; Returns a Rosette (concrete or symbolic value) that is the 
; result of replacing the symbolic constant x with v in expr.
(define (substitute expr x v)
  (evaluate expr (sat (hash x v))))
