#lang typed/racket

(require "types.rkt")
(require typed/racket/unsafe)

(unsafe-require/typed racket/hash
                      [hash-union (∀ (α β) ([(Immutable-HashTable α β)]
                                            #:rest (Immutable-HashTable α β)
                                            . ->* .
                                            (Immutable-HashTable α β)))])

(define-type RewriteEnv (Immutable-HashTable Symbol ArithExpr))

;;
(define (rewrite [term : ArithExpr] [em : Error-Model]) : ArithSetExpr
  (Choices
   (cons (match term
           [(BinOp a op b) (BinOp (rewrite a em) op (rewrite b em))]
           [(or (? integer?) (? symbol?)) term])
         (rewrite* term em))))

;;
(define (rewrite* [term : ArithExpr] [em : Error-Model]) : (Listof ArithSetExpr)
  (match em
    ['() '()]
    [(cons (Rewrite-Rule before after) rst)
     (let [(r (rewrite* term rst)) (b (before-matches? before term))] 
       (if b (cons (apply-after b after) r) r))]))

;; Given the left hand side of a rewrite rule and a coressponding AST, walks over the data
;; sturctures in parrallel and checks to ensure that they match. If they do match a mapping of
;; names to AST fragments is returned, otherwise #f
(define (before-matches? [before : ArithExpr-Rule] [term : ArithExpr]) : (Option RewriteEnv)
  (match* (before term)
    [((Subst s) _) (hash s term)]
    [((BinOp a0 op0 b0) (BinOp a1 op1 b1))
     (let [(a (before-matches? a0 a1)) (b (before-matches? b0 b1)) (op (before-matches? op0 op1))]
       (and a b op (hash-union a b op)))]
    [(_ _) (and (equal? before term) #hash())]))

;; Given a rewrite environmnet containing parts of the original AST at given symbols, plugs the
;; apporpriate parts of the AST in for the given substitution holes in the rewrite rule
(define (apply-after [env : RewriteEnv] [after : ArithSetExpr-Rule]) : ArithSetExpr
  (match after
    [(Subst s) (hash-ref env s)]
    [(BinOp a op b)
     (BinOp (apply-after env a) (cast (apply-after env op) ArithSetOp) (apply-after env b))]
    [(Choices clauses) (Choices (map (λ ([a : ArithSetExpr-Rule]) (apply-after env a)) clauses))]
    [(or (? integer?) (? symbol?)) after]))

;; --------------------------------------------------------------------------------------------------
(module+ test
  (require typed/rackunit)

  
  (define EM : Error-Model (list (Rewrite-Rule
                                  (BinOp (Subst 'a) '+ (Subst 'b))
                                  (BinOp (Subst 'a) (Choices '(- * /)) (Subst 'b)))))
  
  (check-equal? (rewrite (BinOp 3 '+ 4) EM)
                '#s(Choices (#s(BinOp #s(Choices (3)) + #s(Choices (4)))
                             #s(BinOp 3 #s(Choices (- * /)) 4)))))

