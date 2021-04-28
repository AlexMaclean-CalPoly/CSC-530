#lang typed/racket

(provide (all-defined-out))

(require "types.rkt")

;; Returns a nice pretty string representation of a logical expression
(define (logic-str [l : Logic]) : String
  (match l
    [(? boolean?) (if l "true" "false")]
    [(InvariantL id) (~a id)]
    [(NotL arg) (format "¬(~a)" (logic-str arg))]
    [(ImpliesL left right) (format "~a => ~a" (logic-str left) (logic-str right))]
    [(ConjunctionL clauses) (format "(~a)" (string-join (map logic-str clauses) " ∧ "))]
    [(DisjunctionL clauses) (format "(~a)" (string-join (map logic-str clauses) " ∨ "))]
    [(SubstitutionL what for in) (format "(~a)[~a/~a]" (logic-str in) (logic-str what) for)]
    [(Vect ))

;; Joins multiple logical expression string representations with newlines
(define (logic-str* [ls : (Listof Logic)]) : String
  (string-join (map logic-str ls) "\n"))
