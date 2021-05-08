#lang typed/racket

(require "types.rkt")

(define EM : Error-Model '(#s(Rewrite-Rule #s(Arith a + b) #s(Arith a #s(Choices (- * /)) b))))

;;
(define (rewrite [term : ArithExpr] [em : Error-Model]) : ArithExpr~
  (Choices
   (cons (match term
           [(BinOp a op b) (BinOp (rewrite a em) op (rewrite b em))]
           [(or (? integer?) (? symbol?)) term])
         (rewrite* ))))

;;
(define (rewrite* [term : ArithExpr] [em : Error-Model]) : (Listof Arith-Set-Expr)
  (match em
    ['() '()]
    [(cons (Rewrite-Rule before after) rst)
     (let [(r (rewrite* term rst)) (b (before-matches? before term))]
       (if b (cons (apply-after term b) r) r))]))



