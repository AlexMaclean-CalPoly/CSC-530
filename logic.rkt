#lang typed/racket

(provide logic->string logic->sexp vect->sexp)

(require "types.rkt" "vector.rkt")

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
    [(? vect?) (format "(~a >= 0)" (vect->string l))]))

(define (vect->string [v : Vect]) : String
  "Unsupported")

;;(format "(~a)"
;;;          (string-join
 ;;          (hash-map v (λ ([var : Variable] [val : (U Integer Te)])
       ;;                  (format "(~a * ~a)" var
 ;;                                (if (integer? val) val (vect->string val))))) " + "))

(define (logic->sexp [l : Logic]) : Sexp
  (match l
    [(? boolean?) (if l 'true 'false)]
    [(InvariantL id) id]
    [(NotL arg) `(not ,(logic->sexp arg))]
    [(ImpliesL left right) `(=> ,(logic->sexp left) ,(logic->sexp right))]
    [(ConjunctionL clauses) `(and . ,(map logic->sexp clauses))]
    [(DisjunctionL clauses) `(or . ,(map logic->sexp clauses))]
    [(SubstitutionL what for in) `(subst ,(logic->sexp what) ,for ,(logic->sexp in))]
    [(? vect?) `(>= ,(vect->sexp l) 0)]))

(define (vect->sexp [v : Vect]) : Sexp
  (define terms (match v
                  [(VectI t) (hash-map t term->sexp)]
                  [(VectX t) (hash-map t (λ ([var : Variable] [coef : TermsI])
                                           (term->sexp var (vect->sexp (VectI coef)))))]))
  (if ((length terms) . > . 1) `(+ . ,terms) (first terms)))

(define (term->sexp [var : Variable] [val : Sexp]) : Sexp
  (match* (var val)
    [(1 v) v]
    [(v 1) v]
    [(_ _) `(* ,var ,val)]))

