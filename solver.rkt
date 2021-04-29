#lang typed/racket/no-check

(provide simplify)

(require "types.rkt" "vector.rkt")

(define (simplify [l : Logic]) : Logic
  (remove-not (remove-subst (remove-implies l)) #t))

(define (remove-implies [l : Logic]) : Logic
  (match l
    [(ImpliesL left right) (DisjunctionL (list (NotL (remove-implies left)) (remove-implies right)))]
    [(ConjunctionL clauses) (ConjunctionL (map remove-implies clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map remove-implies clauses))]
    [(NotL var) (NotL (remove-implies var))]
    [(SubstitutionL w f i) (SubstitutionL w f (remove-implies i))]
    [(or (? hash?) (? boolean?)) l]
    [(? InvariantL?) (error 'remove-implies "No invariants should be present")]))

(define (remove-subst [l : Logic]) : Logic
  (match l
    [(SubstitutionL w f i) (subst-vect w f (remove-subst i))]
    [(ConjunctionL clauses) (ConjunctionL (map remove-subst clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map remove-subst clauses))]
    [(NotL var) (NotL (remove-subst var))]
    [(or (? hash?) (? boolean?)) l]
    [(or (? InvariantL?) (? ImpliesL?))
     (error 'remove-subst "No invariants or implies should be present")]))

(define (subst-vect [what : Vect] [for : Symbol] [in : Logic]) : Logic
  (match in
    [(? hash?) (vect-subst what for in)]
    [(ConjunctionL clauses) (ConjunctionL (map (λ ([c : Logic]) (subst-vect what for c)) clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map (λ ([c : Logic]) (subst-vect what for c)) clauses))]
    [(NotL var) (NotL (subst-vect what for var))]
    [(? boolean?) in]
    [(or (? InvariantL?) (? ImpliesL?) (? SubstitutionL?))
     (error 'remove-subst "No invariants or implies should be present")]))

(define (remove-not [l : Logic] [t : Boolean]) : Logic
  (match l
    [(NotL var) (remove-not var (not t))]
    [(? boolean?) (equal? t l)]
    [(ConjunctionL clauses) ((if t ConjunctionL DisjunctionL) (map (lambda ([c : Logic]) (remove-not c t)) clauses))]
    [(DisjunctionL clauses) ((if t DisjunctionL ConjunctionL) (map (lambda ([c : Logic]) (remove-not c t)) clauses))]
    [(? hash?) (if t l (negate-vect l))]))


                                


