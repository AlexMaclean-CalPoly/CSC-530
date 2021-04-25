#lang typed/racket
(provide (all-defined-out))
(require "control-graph.rkt")
(require "parser.rkt")
(require "logic.rkt")

(define (get-invariants [cfg : CFG]) : (Listof Logic)
  (append-map
   (λ ([cp : Cut-Point])
     (map ImpliesL
          (map (λ ([x : (Listof Logic)])
                 (ConjunctionL (cons (if (start-point? cp) true 'I) x)))
               (get-conditions (hash-ref cfg cp) cfg (make-immutable-hash)))
          (map (λ ([path : (Listof Stmt)])
                 (omega path 'I))
               (get-paths (hash-ref cfg cp) cfg))))
   (filter Cut-Point? (hash-keys cfg))))

(define (start-point? [c : Cut-Point]) : Boolean
  (equal? (Cut-Point-name c) 'start))

(define (omega [tau : (U Stmt (Listof Stmt))] [I : Logic]) : Logic
  (match tau
    [(Assert p) (ConjunctionL (list p I))]
    [(Assume p) (ImpliesL p I)]
    [(Assign-Unknown x) (SubstitutionL (gensym 'r) x I)]
    [(Assign x e) (SubstitutionL e x I)]
    [(cons S1 S2) (omega S1 (omega S2 I))]
    ['() I]))

(define (subst-exp [e : Exp] [env : (HashTable Symbol Exp)]) : Exp
  (match e
    [(? symbol? s) (hash-ref env s (thunk s))]
    [(? integer?) e]
    [(Prim op a b) (Prim op (subst-exp a env) (subst-exp b env))]))

(define (get-conditions [tau : Any] [cfg : CFG] [env : (HashTable Symbol Exp)])
  : (Listof (Listof Logic)) 
  (match tau
    [(cons (Assign var val) rst) (get-conditions rst cfg (hash-set env var (subst-exp val env)))]
    [(list (GoTo (? Cut-Point?))) '(())]
    [(list (GoTo label)) (get-conditions (hash-ref cfg label) cfg env)]
    [(Conditional pred t f)
     (append (map (λ ([c : (Listof Logic)]) (cons (subst-exp pred env) c))
                  (get-conditions (list (GoTo t)) cfg env))
             (map (λ ([c : (Listof Logic)]) (cons (NotL (subst-exp pred env)) c))
                  (get-conditions (list (GoTo f)) cfg env)))]
    [(cons _ rst) (get-conditions rst cfg env)]))

;; Given the CNode after a cutpoint, returns a list of paths to other cut points
(define (get-paths [c : CNode] [cfg : CFG]) : (Listof (Listof Stmt))
  (match c
    [(Conditional test t f) (append (get-paths (list (GoTo t)) cfg) (get-paths (list (GoTo f)) cfg))]
    [(list (GoTo (? Cut-Point?))) '(())]
    [(list (GoTo label)) (get-paths (hash-ref cfg label) cfg)]
    [(cons stmt rst) (map (λ ([path : (Listof Stmt)]) (cons (cast stmt Stmt) path))
                          (get-paths rst cfg))]))

