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
    [(SubstitutionL what for in) (format "(~a)[~a/~a]" (logic-str in) (vect-str what) for)]
    [(? hash?) (format "(~a >= 0)" (vect-str l))]))

(define (vect-str [v : Vect]) : String
  (format "(~a)"
          (string-join
           (hash-map v (λ ([var : Variable] [val : (U Integer Vect-i)])
                         (format "(~a * ~a)" var (if (integer? val) val (vect-str val))))) " + ")))

;; Joins multiple logical expression string representations with newlines
(define (logic-str* [ls : (Listof Logic)]) : String
  (string-join (map logic-str ls) "\n"))

(define (logic->sexp [l : Logic]) : Sexp
  (match l
    [(? boolean?) (if l 'true 'false)]
    [(InvariantL id) id]
    [(NotL arg) `(not ,(logic->sexp arg))]
    [(ImpliesL left right) `( => ,(logic->sexp left) ,(logic->sexp right))]
    [(ConjunctionL clauses) `(and . ,(map logic->sexp clauses))]
    [(DisjunctionL clauses) `(or . ,(map logic->sexp clauses))]
    [(SubstitutionL what for in) `(subst ,(logic->sexp what) ,for ,(logic->sexp in))]
    [(? hash?) (format "(~a >= 0)" (vect-str l))]))

(define (vect->sexp [v : Vect]) : Sexp
  `(+ . ,(hash-map v (lambda ([var : Variable] [val : (U Integer Vect-i)]) `(* ,var ,(if (integer? val) val (vect->sexp val)))))))
                    

