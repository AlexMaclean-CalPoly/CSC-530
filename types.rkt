#lang typed/racket
(provide (all-defined-out))

;; Abstract Syntax Tree ------------------------------------------------------------------------------

(define-type Program (Listof Stmt))

(define-type Stmt (U While If Assert Assume Assign))
(struct While ([test : Logic] [body : Program]) #:transparent)
(struct If ([test : Logic] [body : Program]) #:transparent)
(struct Assert ([test : Logic]) #:transparent)
(struct Assume ([test : Logic]) #:transparent)
(struct Assign ([var : Symbol] [val : VectI]) #:transparent)

;; Predicates ----------------------------------------------------------------------------------------

(define-type Vect (U VectX VectI))
(struct VectX ([terms : TermsX]) #:transparent)
(struct VectI ([terms : TermsI]) #:transparent)
(define-type TermsI (Immutable-HashTable Variable Integer))
(define-type TermsX (Immutable-HashTable Variable TermsI))
(define-type Variable (U Symbol One))

(define-type Logic (U Vect ImpliesL ConjunctionL DisjunctionL SubstitutionL NotL Boolean InvariantL))
(struct ImpliesL ([left : Logic] [right : Logic]) #:transparent)
(struct ConjunctionL ([clauses : (Listof Logic)]) #:transparent)
(struct DisjunctionL ([clauses : (Listof Logic)]) #:transparent)
(struct SubstitutionL ([what : VectI] [for : Symbol] [in : Logic]) #:transparent)
(struct InvariantL ([id : Symbol]) #:transparent)
(struct NotL ([arg : Logic]) #:transparent)

;; Control Flow Graph --------------------------------------------------------------------------------

(define-type CFG (Mutable-HashTable Label CNode))

(define-type CNode (U Basic-Block Conditional))
(define-type Basic-Block (Listof (U Stmt GoTo)))
(struct GoTo ([next : Label]) #:transparent)
(struct Conditional ([pred : Logic] [t : Label] [f : Label]) #:transparent)

(define-type Label (U Symbol Cut-Point))
(struct Cut-Point ([name : Symbol]) #:transparent)

;; Z3 Assertions -------------------------------------------------------------------------------------

(struct Assert= ([v : VectX] [o : (U Zero Symbol)]) #:transparent)
