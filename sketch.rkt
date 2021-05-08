#lang typed/racket/no-check

(require "types.rkt")

(define (translate/Choices [c : (Choices ArithExpr~)] [no-weight? : Boolean]) : StmtSK
  (match c
    [(Choices (list 1st 2nd)) (IfSK (list (ReturnSK 1st)) (list (ReturnSK 2nd)))]
    [(Choices (list 1st rst ..1)) (IfSK (list (ReturnSK 1st)) (list (translate/Choices (Choices rst) #f)))]))

(define (translate [a : ArithExpr~] [funs : (Mutable-HashTable Symbol (Listof StmtSK))]) : StmtSK
  (match a
    [(or (? integer?) (? symbol?)) (ReturnSK a)]
    [(BinOp a op b)
  

(translate/Choices (Choices '(1 2 3)) #t)
    