#lang typed/racket

(provide get-constraints)

(require "types.rkt")

(define (get-constraints [cfg : CFG]) : (Listof Logic)
  (append-map
   (λ ([cp : Cut-Point])
     (map ImpliesL
          (map (λ ([x : Logic])
                 (ConjunctionL (list (if (start-point? cp) #t (InvariantL 'I)) x)))
               (get-conditions (hash-ref cfg cp) cfg))
          (map (λ ([path : (Listof Stmt)])
                 (omega path (InvariantL 'I)))
               (get-paths (hash-ref cfg cp) cfg))))
   (filter Cut-Point? (hash-keys cfg))))

(define (start-point? [c : Cut-Point]) : Boolean
  (equal? (Cut-Point-name c) 'start))

(define (omega [tau : (U Stmt (Listof Stmt))] [I : Logic]) : Logic
  (match tau
    [(Assert p) (ConjunctionL (list p I))]
    [(Assume p) (ImpliesL p I)]
    [(Assign x e) (SubstitutionL e x I)]
    [(cons S1 S2) (omega S1 (omega S2 I))]
    ['() I]))

(define (get-conditions [tau : Any] [cfg : CFG]) : (Listof Logic)
  (match tau
    [(cons (Assign var val) rst)
     (map (λ ([l : Logic]) (SubstitutionL val var l)) (get-conditions rst cfg))]
    [(list (GoTo (? Cut-Point?))) '(#t) ]
    [(list (GoTo label)) (get-conditions (hash-ref cfg label) cfg)]
    [(Conditional pred t f)
     (append (map (λ ([c : Logic]) (ConjunctionL (list pred c)))
                  (get-conditions (list (GoTo t)) cfg))
             (map (λ ([c : Logic]) (ConjunctionL (list (NotL pred) c)))
                  (get-conditions (list (GoTo f)) cfg)))]
    [(cons _ rst) (get-conditions rst cfg)]))

;; Given the CNode after a cut point, returns a list of paths to other cut points
(define (get-paths [c : CNode] [cfg : CFG]) : (Listof (Listof Stmt))
  (match c
    [(Conditional test t f) (append (get-paths (list (GoTo t)) cfg) (get-paths (list (GoTo f)) cfg))]
    [(list (GoTo (? Cut-Point?))) '(())]
    [(list (GoTo label)) (get-paths (hash-ref cfg label) cfg)]
    [(cons stmt rst) (map (λ ([path : (Listof Stmt)]) (cons (cast stmt Stmt) path))
                          (get-paths rst cfg))]))
