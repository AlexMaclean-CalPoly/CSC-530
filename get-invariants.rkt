#lang typed/racket

(require "control-graph.rkt")
(require "parser.rkt")

(struct Implies ([left : Any] [right : Any]) #:transparent)
(struct Junction ([op : (U '^ 'v)] [elts : (Listof Any)]) #:transparent)

(struct Substitution ([what : Any] [for : Symbol] [in : Any]) #:transparent)

(define-type Invariants (Mutable-HashTable Symbol Any))

(define (get-invariants [cfg : CFG]) : Invariants
  (define invariants : Invariants (make-hash))
  (invariants/CutPoint cfg invariants (Cut-Point 'start)))


(define (invariants/CutPoint [cfg : CFG] [invariants : Invariants] [start : Cut-Point]))

(struct rule [I : 'I] [preds : Any])


