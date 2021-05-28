#lang rosette

(require "uf.rkt")

; This module implements some basic operations on matrices, 
; represented as a list of rows (also lists) of 32-bit integers.
; For example, the 2 x 2 matrix [ 3 4; 5 6 ] is represented as follows:
; (list (list (bv 3 32) (bv 4 32))
;       (list (bv 5 32) (bv 6 32)))
; The module also includes two verification queries that currently take a
; long time to run, even on small matrices. The goal is to make them run
; much faster by judiciously replacing some operations with uninterpreted
; functions, as described in Section 3 of Homework 2.
;
; Credit: this problem was inspired by the following bug report
;  https://github.com/emina/rosette/issues/115.

; Returns the dot product of two vectors,
; represented as a list of int?.
; Note that bvadd and bvmul each take 1 or more arguments.
(define (dot-product u v)
  (apply bvadd (map bvmul u v)))

; Returns the transpose of the matrix A.
(define (transpose A)
  (apply map list A))

; Returns the dot product of the matrices A and B.
; Assumes that the dimensions of the two matrices
; are compatible, i.e., A is m x k and B is k x n.
(define (multiply A B)
  (for/list ([A-row A])
    (for/list ([B-row (transpose B)])
      (dot-product A-row B-row))))

; Returns an n x n matrix of fresh symbolic int? constants.
(define (symbolic-matrix n)
  (for/list ([i n])
    (for/list ([j n])
      (define-symbolic* x (bitvector 32))
      x)))

; Verifies that A * B = B * A for n x n matrices A and B with equal contents.
(define (query-1 n)
  (printf "A = B => A*B = B*A for ~a x ~a matrices.\n" n n)
  (define A (symbolic-matrix n))
  (define B (symbolic-matrix n))
  (time
   (verify
    #:assume    (assert (equal? A B)) 
    #:guarantee (assert (equal? (multiply A B) (multiply B A))))))

; Verifies that transpose(A*A) = B*B for n x n matrices A and B such that
; transpose of A and B have equal contents.
(define (query-2 n)
  (printf "B = transpose(A) => transpose(A*A) = B*B for ~a x ~a matrices.\n" n n)
  (define A (symbolic-matrix n))
  (define B (symbolic-matrix n))
  (time
   (verify
    #:assume    (assert (equal? B (transpose A))) 
    #:guarantee (assert (equal? (transpose (multiply A A)) (multiply B B))))))

; The following tiny queries take seconds to verify.
; Increasing the size argument by 1 causes both queries to
; not terminate even after a few minutes!
(query-1 3)
(query-2 3)
