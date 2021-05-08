#lang typed/racket/no-check

(require "types.rkt")
(require racket/hash) 

(define EM : Error-Model '(#s(Rewrite-Rule #s(BinOp #s(Subst a) + #s(Subst b)) #s(BinOp #s(Subst a) #s(Choices (- * /)) #s(Subst b)))))

;;
(define (rewrite [term : ArithExpr] [em : Error-Model]) : ArithExpr~
  (Choices
   (cons (match term
           [(BinOp a op b) (BinOp (rewrite a em) op (rewrite b em))]
           [(or (? integer?) (? symbol?)) term])
         (rewrite* term em))))

;;
(define (rewrite* [term : ArithExpr] [em : Error-Model]) : (Listof ArithExpr~)
  (match em
    ['() '()]
    [(cons (Rewrite-Rule before after) rst)
     (let [(r (rewrite* term rst)) (b (before-matches? before term))] 
       (if b (cons (apply-after b after) r) r))]))

(define (before-matches? [before : (ArithExpr* Subst)] [term : ArithExpr]) : (U False (Immutable-HashTable Subst ArithExpr))
  (match* (before term)
    [((? Subst?) _) (make-immutable-hash (list (cons before term)))]
    [((BinOp a0 op0 b0) (BinOp a1 op1 b1)) (let [(a (before-matches? a0 a1)) (b (before-matches? b0 b1)) (op (before-matches? op0 op1))]
                                             (and a b op (hash-union a b op)))]
    [(_ _) (and (equal? before term) (make-immutable-hash '()))]))

(define (apply-after [env : (Immutable-HashTable Subst ArithExpr)] [after : (ArithExpr~* Subst)]) : ArithExpr~
  (match after
    [(? Subst?) (hash-ref env after)]
    [(BinOp a op b) (BinOp (apply-after env a) (cast (apply-after env op) ArithOp~) (apply-after env b))]
    [(Choices clauses) (Choices (map (lambda ([a : (ArithExpr~* Subst)]) (apply-after env a)) clauses))]
    [(or (? integer?) (? symbol?)) after]))
                                           
(rewrite (BinOp 3 '+ 4) EM)

