#lang racket

(provide rewrite)

(require racket/hash "types.rkt")

;; Recursively rewrites a mPy program to an ~mPy~ program using the given error model
(define (rewrite term error-model)
  (Choices
   (cons (match term
           [(BinOp a op b) (BinOp (rewrite a error-model) op (rewrite b error-model))]
           [(Not v) (Not (rewrite v error-model))]
           [(If f t e) (If (rewrite f error-model) (rewrite t error-model) (rewrite e error-model))]
           [(Assign var val) (Assign var (rewrite val error-model))]
           [(Return v) (Return (rewrite v error-model))]
           [(cons s1 s2) (cons (rewrite s1 error-model) (rewrite s2 error-model))]
           [(or (? integer?) (? symbol?) (? boolean?) (? Op?)) term])
         (filter (negate void?) (map (λ (rule) (apply-rule term rule)) error-model)))))

;; Given a term and of the source and a single rewrite rule, rewrites it using the rule if it matches
;; the rule and returns void otherwise
(define (apply-rule term rule)
  (define matches? (before-matches? (Rewrite-Rule-before rule) term))
  (if matches? (apply-after matches? (Rewrite-Rule-after rule)) (void)))

;; Given the left hand side of a rewrite rule and a corresponding AST, walks over the data
;; structures in parallel and checks to ensure that they match. If they do match a mapping of
;; names to AST fragments is returned, otherwise #f
(define (before-matches? before term)
  (match* (before term)
    [((Subst s) _) (hash s term)]
    [((BinOp a0 op0 b0) (BinOp a1 op1 b1))
     (hash-union-if (before-matches? a0 a1) (before-matches? op0 op1) (before-matches? b0 b1))]
    [((Not v0) (Not v1)) (before-matches? v0 v1)]
    [((If f0 t0 e0) (If f1 t1 e1))
     (hash-union-if (before-matches? f0 f1) (before-matches? t0 t1) (before-matches? e0 e1))]
    [((Assign a0 b0) (Assign a1 b1)) (hash-union-if (before-matches? a0 a1) (before-matches? b0 b1))]
    [((Return a0) (Return a1)) (before-matches? a0 a1)]
    [((cons a0 b0) (cons a1 b1))
     (hash-union-if (before-matches? a0 a1) (before-matches? b0 b1))]
    [(_ _) (and (equal? before term) #hash())]))

;; Given a rewrite environment containing parts of the original AST at given symbols, plugs the
;; appropriate parts of the AST in for the given substitution holes in the rewrite rule
(define (apply-after env after)
  (match after
    [(Subst s) (hash-ref env s)]
    [(BinOp a op b) (BinOp (apply-after env a) (apply-after env op) (apply-after env b))]
    [(Not a) (Not (apply-after env a))]
    [(If f t e) (If (apply-after env f) (apply-after env t) (apply-after env e))]
    [(Assign var val) (Assign (apply-after env var) (apply-after env val))]
    [(Return v) (Return (apply-after env v))]
    [(cons s1 s2) (cons (apply-after env s1) (apply-after env s2))]
    [(Choices clauses) (Choices (map (λ (a) (apply-after env a)) clauses))]
    [(or (? integer?) (? symbol?) (? boolean?) (? Op?)) after]))

;; Given some things which are either hashes or #f, returns the union if they are all hashes and
;; #f if any of them are #f
(define (hash-union-if . hashes?)
  (and (andmap identity hashes?) (apply hash-union hashes?)))

;; --------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)


  (define EM (list (Rewrite-Rule
                    (BinOp (Subst 'a) '+ (Subst 'b))
                    (BinOp (Subst 'a) (Choices '(- * /)) (Subst 'b)))))

  (check-equal? (rewrite (BinOp 3 '+ 4) EM)
                '#s(Choices (#s(BinOp #s(Choices (3)) + #s(Choices (4)))
                             #s(BinOp 3 #s(Choices (- * /)) 4)))))

