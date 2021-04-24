#lang typed/racket

(require parser)

(define-type CFG (Mutable-HashTable Label CNode))
(define-type CNode (U Basic-Block Conditional))

(struct Basic-Block ([program : Program] [next : Label]) #:transparent)

(struct Conditional ([pred : Exp] [t : Label] [f : Label]))

(struct Cut-Point ([name : Symbol]))

(define-type Label (U Symbol Cut-Point))

(define (make-CFG [cfg : CFG] [p : Program]) : CNode
  (match p
    [(cons (If test body) rest)))
