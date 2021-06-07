#lang typed/racket

(provide simplify)

(require "types.rkt" "vector.rkt" "logic.rkt")

(define (simplify [l : Logic]) : Logic
  (define simplified (flatten-l (remove-not (remove-subst (remove-implies l)) #t)))
  (if (boolean? simplified) simplified (flatten-l (to-CNF (flatten-l simplified)))))

;; A logic at this point may have: ImpliesL ConjunctionL DisjunctionL SubstitutionL NotL Vect Boolean
(define (remove-implies [l : Logic]) : Logic
  (match l
    [(ImpliesL left right) (DisjunctionL (list (NotL (remove-implies left)) (remove-implies right)))]
    [(ConjunctionL clauses) (ConjunctionL (map remove-implies clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map remove-implies clauses))]
    [(NotL var) (NotL (remove-implies var))]
    [(SubstitutionL w f i) (SubstitutionL w f (remove-implies i))]
    [(or (? vect?) (? boolean?)) l]))

;; A logic at this point may have: ConjunctionL DisjunctionL SubstitutionL NotL Vect Boolean
(define (remove-subst [l : Logic]) : Logic
  (match l
    [(SubstitutionL w f i) (subst-vect w f (remove-subst i))]
    [(? VectI?) (to-vect-x l)]
    [(ConjunctionL clauses) (ConjunctionL (map remove-subst clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map remove-subst clauses))]
    [(NotL var) (NotL (remove-subst var))]
    [(or (? VectX?) (? boolean?)) l]))

;; A logic at this point may have: ConjunctionL DisjunctionL SubstitutionL NotL Vect Boolean
(define (subst-vect [what : VectI] [for : Symbol] [in : Logic]) : Logic
  (match in
    [(? VectX?) (vect-subst what for in)]
    [(? VectI?) (vect-subst what for (to-vect-x in))]
    [(ConjunctionL clauses) (ConjunctionL (map (λ ([c : Logic]) (subst-vect what for c)) clauses))]
    [(DisjunctionL clauses) (DisjunctionL (map (λ ([c : Logic]) (subst-vect what for c)) clauses))]
    [(NotL var) (NotL (subst-vect what for var))]
    [(? boolean?) in]))

;; A logic at this point may have: ConjunctionL DisjunctionL NotL VectX Boolean
(define (remove-not [l : Logic] [t : Boolean]) : Logic
  (match l
    [(NotL var) (remove-not var (not t))]
    [(? boolean?) (equal? t l)]
    [(ConjunctionL clauses)
     ((if t ConjunctionL DisjunctionL) (map (λ ([c : Logic]) (remove-not c t)) clauses))]
    [(DisjunctionL clauses)
     ((if t DisjunctionL ConjunctionL) (map (λ ([c : Logic]) (remove-not c t)) clauses))]
    [(? VectX?) (if t l (negate-vect l))]))

;; A logic at this point may have: ConjunctionL DisjunctionL VectX Boolean
(define (flatten-l [l : Logic]) : Logic
  (match l
    [(or (? boolean?) (? VectX?)) l]
    [(ConjunctionL clauses)
     (let [(flat-clauses (remove* '(#t) (map flatten-l clauses)))]
       (if (member #f flat-clauses) #f
           (ConjunctionL (append (filter (negate ConjunctionL?) flat-clauses)
                                 (append-map ConjunctionL-clauses
                                             (filter ConjunctionL? flat-clauses))))))]
    [(DisjunctionL clauses)
     (let [(flat-clauses (remove* '(#f) (map flatten-l clauses)))]
       (if (member #t flat-clauses) #t
           (DisjunctionL (append (filter (negate DisjunctionL?) flat-clauses)
                                 (append-map DisjunctionL-clauses
                                             (filter DisjunctionL? flat-clauses))))))]))

;; A logic at this point may have: ConjunctionL DisjunctionL VectX
(define (to-CNF [l : Logic]) : Logic
  (ConjunctionL (to-CNF-helper l)))

(define (to-CNF-helper [l : Logic]) : (Listof Logic)
  (match l
    [(? VectX?) (list l)]
    [(ConjunctionL clauses) (append-map to-CNF-helper clauses)]
    [(DisjunctionL clauses)
     (map DisjunctionL (apply cartesian-product (map to-CNF-helper clauses)))]))



