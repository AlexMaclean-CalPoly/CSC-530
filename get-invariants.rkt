#lang typed/racket/no-check


(require "control-graph.rkt")
(require "parser.rkt")

(struct Implies ([left : Any] [right : Any]) #:transparent)
(struct Junction ([op : (U '∧ 'v)] [elts : (Listof Any)]) #:transparent)

(struct Substitution ([what : Any] [for : Symbol] [in : Any]) #:transparent)

(define-type Invariants (Mutable-HashTable Symbol Any))

(define (get-invariants [cfg : CFG]) : Any
  (append-map (lambda ([cp : Cut-Point]) (omega (hash-ref cfg cp) 'I cfg)) (filter Cut-Point? (hash-keys cfg))))

(define (omega [tau : Any] [I : Any] [cfg : CFG]) : Any
  (match tau
    [(Assert p) (list (Junction '∧ (list p I)))]
    [(Assume p) (list (Implies p I))]
    ;;[(Assign var (void)) (Substitution (gensym 'r) var I)]
    [(Assign var val) (list (Substitution val var I))]
    [(cons S1 S2) (append-map (lambda ([t : Any]) (omega S1 t cfg)) (omega S2 I cfg))]
    [(GoTo (Cut-Point _)) (list I)]
    [(GoTo label) (omega (hash-ref cfg label) I cfg)]
    [(Conditional pred t f) (append (omega (hash-ref cfg t) I cfg) (omega (hash-ref cfg f) I cfg))]
    ['() (list I)]))

; (define (get-conditions [tau : Any] [cfg : CFG]) : Any

(define (display-invariant [I : Any]) : Any
  (match I
    [(Implies left right) (format "~a => ~a" (display-invariant left) (display-invariant right))]
    [(Junction op elts) (string-join (map display-invariant elts) (format " ~a " op))]
    [(Substitution what for in) (format "~a[~a/~a]" (display-invariant in) (display-invariant what) (display-invariant for))]
    [(Prim op a b) (format "~a ~a ~a" (display-invariant a) op (display-invariant b))]
    [(? symbol? a) (~a a)]
    [(? integer? a) (~a a)]))

(define (display-invariants [I : Any]) : String
  (string-join (map display-invariant I) "\n"))


(omega (list (Assert 1) (Assign 'x 2)) 'I (make-hash))
(displayln (display-invariants (get-invariants (make-CFG-program
    (list
    (Assign 'x -50)
    (While (Prim '< 'x 0)
           (list
            (Assign 'x (Prim '+ 'x 'y))
            (Assign 'y (Prim '+ 'y 1))))
    (Assert (Prim '> 'y 0)))))))
