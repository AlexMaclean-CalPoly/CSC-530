#lang typed/racket/no-check


(require "control-graph.rkt")
(require "parser.rkt")

(struct Implies ([left : Any] [right : Any]) #:transparent)
(struct Junction ([op : (U '∧ 'v)] [elts : (Listof Any)]) #:transparent)

(struct Substitution ([what : Any] [for : Symbol] [in : Any]) #:transparent)
(struct Not ([elt : Any]) #:transparent)

(define-type Invariants (Mutable-HashTable Symbol Any))

(define (get-invariants [cfg : CFG]) : Any
  (append-map (lambda ([cp : Cut-Point])
                (map Implies
                  (map (lambda ([x : Any]) (Junction '∧ (cons (if (start-point? cp) true 'I) x))) (get-conditions (hash-ref cfg cp) cfg (make-immutable-hash)))
                  (omega (hash-ref cfg cp) 'I cfg))) (filter Cut-Point? (hash-keys cfg))))

(define (start-point? [c : Cut-Point]) : Boolean
  (equal? (Cut-Point-name c) 'start))

(define (omega [tau : Any] [I : Any] [cfg : CFG]) : Any
  (match tau
    [(Assert p) (list (Junction '∧ (list p I)))]
    [(Assume p) (list (Implies p I))]
    ;;[(Assign var (void)) (Substitution (gensym 'r) var I)]
    [(Assign var val) (list (Substitution val var I))]
    [(cons S1 S2) (append-map (lambda ([t : Any]) (omega S1 t cfg)) (omega S2 I cfg))]
    [(GoTo (Cut-Point _)) (list I)]
    [(GoTo label) (omega (hash-ref cfg label) I cfg)]
    [(Conditional pred t f) (append (omega (GoTo t) I cfg) (omega (GoTo f) I cfg))]
    ['() (list I)]))

(define (subst-exp [e : Exp] [env : (HashTable Symbol Exp)]) : Exp
  (match e
    [(? symbol? s) (hash-ref env s s)]
    [(? integer?) e]
    [(Prim op a b) (Prim op (subst-exp a env) (subst-exp b env))]))

(define (get-conditions [tau : Any] [cfg : CFG] [env : (HashTable Symbol Exp)]) : Any 
  (match tau
    [(cons (Assign var val) rst) (get-conditions rst cfg (hash-set env var (subst-exp val env)))]
    [(cons (GoTo (? Cut-Point?)) '()) '(())]
    [(cons (GoTo label) '()) (get-conditions (hash-ref cfg label) cfg env)]
    [(Conditional pred t f)
     (append (map (lambda ([c : Any]) (cons pred c))
                  (get-conditions (list (GoTo t)) cfg env))
             (map (lambda ([c : Any]) (cons (Not pred) c))
                  (get-conditions (list (GoTo f)) cfg env)))]
    [(cons _ rst) (get-conditions rst cfg env)]))

(define (display-invariant [I : Any]) : Any
  (match I
    [(Implies left right) (format "~a => ~a" (display-invariant left) (display-invariant right))]
    [(Junction op elts) (string-join (map display-invariant elts) (format " ~a " op))]
    [(Substitution what for in)
     (format "~a[~a/~a]" (display-invariant in) (display-invariant what) (display-invariant for))]
    [(Prim op a b) (format "~a ~a ~a" (display-invariant a) op (display-invariant b))]
    [(? symbol? a) (~a a)]
    [(? integer? a) (~a a)]
    [(? boolean? b) (if b "true" "false")]
    [(Not elt) (format "¬(~a)" (display-invariant elt))]))

(define (display-invariants [I : Any]) : String
  (string-join (map display-invariant I) "\n"))

; (omega (list (Assert 1) (Assign 'x 2)) 'I (make-hash))
(displayln (display-invariants (get-invariants (make-CFG-program
    (list
    (Assign 'x -50)
    (While (Prim '< 'x 0)
           (list
            (Assign 'x (Prim '+ 'x 'y))
            (Assign 'y (Prim '+ 'y 1))))
    (Assert (Prim '> 'y 0)))))))
