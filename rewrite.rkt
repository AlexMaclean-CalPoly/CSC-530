#lang typed/racket


(require "types.rkt")

(define EM : Error-Model (list (Rewrite-Rule (Arith 'a '+ 'b) (Arith 'a (Choices (list '- '* '/)) 'b))))

(define (rewrite [term : Arith-Expr] [em : Error-Model]) : Arith-Set-Expr
  (Choices
   (cons (match term
           [(Arith a op b) (Arith (rewrite a em) op (rewrite b em))]
           [(or (? integer?) (? symbol?)) term])
         (rewrite*



(define (rewrite* [term : Arith-Expr] [em : Error-Model]) : (Listof Arith-Set-Expr)
  (match em
    ['() '()]
    [(cons (Rewrite-Rule before after) rst) (let [(r (rewrite* term rst)) (b (before-matches? before term))]
                                              (if b (cons (apply-after term b) r) r))]))



      