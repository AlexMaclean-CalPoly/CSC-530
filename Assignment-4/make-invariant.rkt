#lang typed/racket

(provide make-invariant extract-vars subst-invariant subst-constant)

(require "types.rkt" "vector.rkt")

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
    [(? VectI?) (extract-vars/Vect l)]
    [(? boolean?) '()]))

(define (extract-vars/Vect [v : VectI]) : (Listof Symbol)
  (filter symbol? (hash-keys (VectI-terms v))))

;; ---------------------------------------------------------------------------------------------------

(define (make-invariant [clauses : Integer] [sub-clauses : Integer] [vars : (Listof Symbol)]) : Logic
  (DisjunctionL (for/list ([c clauses])
                  (ConjunctionL (for/list ([s sub-clauses]) (make-sum vars))))))

(define (make-sum [vars : (Listof Symbol)]) : VectX
  (VectX (for/hash : TermsX ([var : Variable (cons 1 vars)])
           (values var (make-const)))))

(define (make-const) : TermsI
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
    [(or (? VectI?) (? boolean?)) in]))

(define (subst-constant [what : Integer] [for : Symbol] [in : Logic]) : Logic
  (match in
    [(ConjunctionL clauses)
     (ConjunctionL (map (λ ([c : Logic]) (subst-constant what for c)) clauses))]
    [(DisjunctionL clauses)
     (DisjunctionL (map (λ ([c : Logic]) (subst-constant what for c)) clauses))]
    [(? VectX?) (vect-subst-const what for in)]))
    

