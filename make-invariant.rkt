#lang typed/racket

(provide make-invariant extract-vars subst-invariant)

(require "types.rkt")

(define (extract-vars [p : Program]) : (Listof Symbol)
  (apply set-union '() (map extract-vars/Stmt p)))

(define (extract-vars/Stmt [s : Stmt]) : (Listof Symbol)
  (match s
    [(Assign var val) (set-union (list var) (extract-vars/Vect val))]
    [(Assert p) (extract-vars/Logic p)]
    [(Assume p) (extract-vars/Logic p)]
    [(While test body) (set-union (extract-vars/Logic test) (extract-vars body))]
    [(If test body) (set-union (extract-vars/Logic test) (extract-vars body))]))

(define (extract-vars/Logic [l : Logic]) : (Listof Symbol)
  (match l
    [(ConjunctionL clauses) (apply set-union '() (map extract-vars/Logic clauses))]
    [(DisjunctionL clauses) (apply set-union '() (map extract-vars/Logic clauses))]
    [(NotL var) (extract-vars/Logic var)]
    [(? hash?) (extract-vars/Vect l)]
    [(? boolean?) '()]))

(define (extract-vars/Vect [v : (U Vect-x Vect-i)]) : (Listof Symbol)
  (filter symbol? (hash-keys v)))

;; ---------------------------------------------------------------------------------------------------

(define (make-invariant [clauses : Integer] [sub-clauses : Integer] [vars : (Listof Symbol)]) : Logic
  (DisjunctionL
   (build-list clauses (λ (_)
                         (ConjunctionL
                          (build-list sub-clauses (λ (_) (make-sum vars))))))))

(define (make-sum [vars : (Listof Symbol)]) : Vect-x
  (make-immutable-hash
   (cons (cons 1 (make-const)) (map (λ ([v : Symbol]) (cons v (make-const))) vars))))

(define (make-const) : Vect-i
  (make-immutable-hash (list (cons (gensym 'u) 1))))

;; ---------------------------------------------------------------------------------------------------

(define (subst-invariant [what : Logic] [for : InvariantL] [in : Logic]) : Logic
  (match in
    [(? InvariantL?) (if (equal? for in) what in)]
    [(ImpliesL left right)
     (ImpliesL (subst-invariant what for left) (subst-invariant what for right))]
    [(ConjunctionL clauses)
     (ConjunctionL (map (λ ([c : Logic]) (subst-invariant what for c)) clauses))]
    [(DisjunctionL clauses)
     (DisjunctionL (map (λ ([c : Logic]) (subst-invariant what for c)) clauses))]
    [(NotL var) (NotL (subst-invariant what for var))]
    [(SubstitutionL w f i) (SubstitutionL w f (subst-invariant what for i))]
    [(or (? hash?) (? boolean?)) in]))

