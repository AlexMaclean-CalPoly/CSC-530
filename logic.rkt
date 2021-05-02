#lang typed/racket

(provide logic->string logic->sexp)

(require "types.rkt")

;; Returns a nice pretty string representation of a logical expression
(define (logic->string [l : Logic]) : String
  (match l
    [(? boolean?) (if l "true" "false")]
    [(InvariantL id) (~a id)]
    [(NotL arg) (format "¬(~a)" (logic->string arg))]
    [(ImpliesL left right) (format "~a => ~a" (logic->string left) (logic->string right))]
    [(ConjunctionL clauses) (format "(~a)" (string-join (map logic->string clauses) " ∧ "))]
    [(DisjunctionL clauses) (format "(~a)" (string-join (map logic->string clauses) " ∨ "))]
    [(SubstitutionL what for in) (format "(~a)[~a/~a]" (logic->string in) (vect->string what) for)]
    [(? hash?) (format "(~a >= 0)" (vect->string l))]))

(define (vect->string [v : Vect]) : String
  (format "(~a)"
          (string-join
           (hash-map v (λ ([var : Variable] [val : (U Integer Vect-i)])
                         (format "(~a * ~a)" var
                                 (if (integer? val) val (vect->string val))))) " + ")))

(define (logic->sexp [l : Logic]) : Sexp
  (match l
    [(? boolean?) (if l 'true 'false)]
    [(InvariantL id) id]
    [(NotL arg) `(not ,(logic->sexp arg))]
    [(ImpliesL left right) `(=> ,(logic->sexp left) ,(logic->sexp right))]
    [(ConjunctionL clauses) `(and . ,(map logic->sexp clauses))]
    [(DisjunctionL clauses) `(or . ,(map logic->sexp clauses))]
    [(SubstitutionL what for in) `(subst ,(logic->sexp what) ,for ,(logic->sexp in))]
    [(? hash?) `(>= ,(vect->sexp l) 0)]))

(define (vect->sexp [v : Vect]) : Sexp
  `(+ . ,(hash-map v (λ ([var : Variable] [val : (U Integer Vect-i)])
                       `(* ,var ,(if (integer? val) val (vect->sexp val)))))))


