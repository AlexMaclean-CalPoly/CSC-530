#lang racket

(require
  (only-in rosette
           solver-assert solver-check solver-clear
           clear-terms! constant boolean? || !
           sat? model)
  (only-in rosette/solver/smt/z3 z3))

(provide 
 cnf/c           ; contract? that recognizes CNF specifications
 interpretation? ; contract? that recognizes CNF interpretations
 solve           ; (->* (cnf/c) [solver?] (or/c #f interpretation?))
)

; We represent a CNF formula as a list of clauses, where each clause is itself 
; a list of non-zero integers.  Positive integers represent positive literals 
; (i.e., positive occurrences of the ith variable), and negative integers 
; represent negative literals.
(define cnf/c (listof (listof (and/c integer? (not/c zero?)))))

; An interpretation is represented as a list of literals, sorted in the 
; increasing order of variable identifiers.  
(define interpretation? (listof integer?))

(define solver (z3 #:logic 'QF_BV))

; Invokes a SAT solver on the given CNF and returns either an interpretation?, 
; if the formula is satisfiable, or #f otherwise.  
(define (solve cnf)
  (solver-clear solver)
  (clear-terms!)
  (solver-assert
   solver
   (for/list ([clause cnf])
     (apply
      ||
      (for/list ([lit clause])
        (if (> lit 0)
            (constant lit boolean?)
            (! (constant (abs lit) boolean?)))))))
  (define sol (solver-check solver))
  (and (sat? sol)
       (sort
        (for/list ([(k v) (model sol)])
          (match k [(constant id _) (if v id (- id))]))
        < #:key abs)))

