#lang typed/racket/no-check


(require "control-graph.rkt")
(require "parser.rkt")

(struct Implies ([left : Any] [right : Any]) #:transparent)
(struct Junction ([op : (U '^ 'v)] [elts : (Listof Any)]) #:transparent)

(struct Substitution ([what : Any] [for : Symbol] [in : Any]) #:transparent)

(define-type Invariants (Mutable-HashTable Symbol Any))

(define (get-invariants [cfg : CFG]) : Any
  (append-map (lambda ([cp : Cut-Point]) (omega (hash-ref cfg cp) 'I cfg)) (filter Cut-Point? (hash-keys cfg))))

(define (omega [tau : Any] [I : Any] [cfg : CFG]) : Any
  (match tau
    [(Assert p) (list (Junction '^ (list p I)))]
    [(Assume p) (list (Implies p I))]
    ;;[(Assign var (void)) (Substitution (gensym 'r) var I)]
    [(Assign var val) (list (Substitution val var I))]
    [(cons S1 S2) (append-map (lambda ([t : Any]) (omega S1 t cfg)) (omega S2 I cfg))]
    [(GoTo (Cut-Point _)) (list I)]
    [(GoTo label) (omega (hash-ref cfg label) I cfg)]
    [(Conditional pred t f) (append (omega (hash-ref cfg t) I cfg) (omega (hash-ref cfg f) I cfg))]
    ['() (list I)]))

(omega (list (Assert 1) (Assign 'x 2)) 'I (make-hash))
(get-invariants (make-CFG-program
    (list
    (Assign 'x -50)
    (While (Prim '< 'x 0)
           (list
            (Assign 'x (Prim '+ 'x 'y))
            (Assign 'y (Prim '+ 'y 1))))
    (Assert (Prim '> 'y 0)))))
