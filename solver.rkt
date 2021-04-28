#lang typed/racket

(provide (all-defined-out))

(require "types.rkt")
(require/typed racket/hash
               [hash-union (Vect Vect [#:combine (Integer Integer -> Integer)] -> Vect)])
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
(define (vect+ [a : Vect] [b : Vect]) : Vect
  (hash-union a b #:combine +))

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
