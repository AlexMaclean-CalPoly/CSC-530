#lang typed/racket

(provide (all-defined-out))
(require "parser.rkt")

(define-type Logic (U Exp ImpliesL ConjunctionL DisjunctionL SubstitutionL NotL Boolean))
(struct ImpliesL ([left : Logic] [right : Logic]) #:transparent)
(struct ConjunctionL ([clauses : (Listof Logic)]) #:transparent)
(struct DisjunctionL ([clauses : (Listof Logic)]) #:transparent)
(struct SubstitutionL ([what : Logic] [for : Symbol] [in : Logic]) #:transparent)
(struct NotL ([arg : Logic]) #:transparent)

;; ---------------------------------------------------------------------------------------------------

;; Returns a nice pretty string representation of a logical expression
(define (logic-str [l : Logic]) : String
  (match l
    [(or (? symbol?) (? integer?)) (~a l)]
    [(? boolean?) (if l "true" "false")]
    [(ImpliesL left right) (format "~a => ~a" (logic-str left) (logic-str right))]
    [(ConjunctionL clauses) (string-join (map logic-str clauses) " ∧ ")]
    [(DisjunctionL clauses) (string-join (map logic-str clauses) " ∨ ")]
    [(SubstitutionL what for in)
     (format "(~a)[~a/~a]" (logic-str in) (logic-str what) (logic-str for))]
    [(Prim op a b) (format "(~a ~a ~a)" (logic-str a) op (logic-str b))]
    [(NotL arg) (format "¬(~a)" (logic-str arg))]))

;; Joins multiple logical expression string representations with newlines
(define (logic-str* [ls : (Listof Logic)]) : String
  (string-join (map logic-str ls) "\n"))
