#lang typed/racket/no-check

(provide (all-defined-out))

(require "types.rkt" "vector.rkt")

(define (subst-invariant [what : Logic] [for : InvariantL] [in : Logic]) : Logic
  (match in
    [(? InvariantL?) (if (equal? for in) what in)]
    [(ImpliesL left right) (ImpliesL (subst-invariant what for left) (subst-invariant what for right))]
    [(ConjunctionL clauses) (ConjunctionL (map (λ ([c : Logic]) (subst-invariant what for c)) clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map (λ ([c : Logic]) (subst-invariant what for c)) clauses))]
    [(NotL var) (NotL (subst-invariant what for var))]
    [(SubstitutionL w f i) (SubstitutionL (subst-invariant what for w) f (subst-invariant what for i))]
    [(or (? hash?) (? boolean?)) in]))

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
    [(or (? InvariantL?) (? ImpliesL?)) (error 'remove-subst "No invariants or implies should be present")]))

(define (subst-vect [what : Vect] [for : Symbol] [in : Logic]) : Logic
  (match in
    [(? hash?) (vect-subst what for in)]
    [(ConjunctionL clauses) (ConjunctionL (map (λ ([c : Logic]) (subst-vect what for c)) clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map (λ ([c : Logic]) (subst-vect what for c)) clauses))]
    [(NotL var) (NotL (subst-vect what for var))]
    [(? boolean?) in]
    [(or (? InvariantL?) (? ImpliesL?) (? SubstitutionL?)) (error 'remove-subst "No invariants or implies should be present")]))


#|
(define (extract-vars [p : Program]) : (Listof Symbol)
  (apply set-union (map extract-vars/Stmt p)))

(define (extract-vars/Stmt [s : Stmt]) : (Listof Symbol)
  (match s
    [(Assign var val) (set-union (list var) (extract-vars/Exp val))]
    [(Assert p) (extract-vars/Exp p)]
    [(Assume p) (extract-vars/Exp p)]
    [(While test body) (set-union (extract-vars/Exp test) (extract-vars body))]
    [(If test body) (set-union (extract-vars/Exp test) (extract-vars body))]))

(define (extract-vars/Exp [e : Exp]) : (Listof Symbol)
  (match e
    [(? symbol?) (list e)]
    [(? integer?) '()]))

(define (subst [what : Logic] [for : Symbol] [in : Logic]) : Logic
  (match in
    [(? symbol?) (if (equal? for in) what in)]
    [(ImpliesL left right) (ImpliesL (subst what for left) (subst what for right))]
    [(ConjunctionL clauses) (ConjunctionL (map (λ ([c : Logic]) (subst what for c)) clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map (λ ([c : Logic]) (subst what for c)) clauses))]
    [(NotL var) (NotL (subst what for var))]
    [(SubstitutionL w f i) (SubstitutionL (subst what for w) f (subst what for i))]
    [(? integer?) in]
    [(? boolean?) in]))

(define (subst-Vect [what : Vect] [for : Symbol] [in : Vect]) : Vect
  (cond
    [(hash-has-key? in for)
     (define coefficent (hash-ref in for))
     (foldr (lambda ([p : (Pairof (U One Symbol) Integer)] [v : Vect]) (vect+ v (* coefficent (cdr p)) (car p))) (hash-remove in for) (hash->list what))]
    [else in]))
|#

#|
; Transforms a logic to remove implies, subst
(define (simplify [e : Logic]) : Logic
  (match e
    [(ImpliesL left right) (DisjunctionL (list (NotL (simplify left)) (simplify right)))]
    [(or (ConjunctionL (list c)) (DisjunctionL (list c))) (simplify c)]
    [(ConjunctionL clauses) (ConjunctionL (map simplify clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map simplify clauses))]
    [(NotL var) (NotL (simplify var))]
    [(SubstitutionL what for in) (subst (simplify what) for (simplify in))]
    [(or (? integer?) (? boolean?) (? symbol?)) e]))
|#
;;(define (make-invariant [clauses : Integer] [sub-clauses : Integer] [vars : (Listof Symbol)]) : Logic
;;  (DisjunctionL
;;   (build-list clauses (λ (_)
;;                         (ConjunctionL
;;                          (build-list sub-clauses (λ (_) (make-sum vars))))))))

;;(define (make-sum [vars : (Listof Symbols)]) : Exp
;;  (Prim '>= (list (Prim '+ (map (λ ([v : Symbol]) (Prim '* (list v (gensym 'u)))) vars)) 0)))
