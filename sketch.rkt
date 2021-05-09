#lang typed/racket/no-check

(require "types.rkt")

(define (translate/Choices [c : (Choices ArithExpr~)] env funs [no-weight? : Boolean]) : StmtSK
  (match c
    [(Choices (list 1st 2nd)) (IfSK (list (ReturnSK 1st)) (list (ReturnSK 2nd)))]
    [(Choices (list 1st rst ..1)) (IfSK (list (ReturnSK 1st)) (list (translate/Choices (Choices rst) #f)))]))

(define (translate [a : ArithExpr~] [env : (Mutable-HashTable Symbol Boolean)] [funs : (Mutable-HashTable Symbol (Listof StmtSK))]) : StmtSK
  (match a
    [(or (? integer?) (? symbol?)) (ReturnSK a)]
    [(BinOp a op b) (BinOp (translate a env funs) (translate op env funs) (translate b env funs))]
    [(Assign var val) (hash-set! env var #t)]
    [(cons s0 s1) (cons (translate s0 env funs) (translate s1 env funs))]
    [(Choices (list 1st)) (translate 1st env funs)]
    [(Choices (list c ..2) (let [(name (gensym 'choices)) (body (translate/Choices c env funs #t)) (params (hash-values env))]
                             (hash-set! funs (FunSK name params body))
                             (CallSK name params))


(define (print-sketch s)
  (match s
    [(BinOp a op b) (format "binOpMT(~a, ~a, ~a);" (print-sketch a) (print-sketch b) (print-sketch op))]
    [(Return var) (format "return ~a;" (print-sketch var))]
    [(If f t e) (format "if ( ~a ) { ~a } else { ~a }" (print-sketch f) (print-sketch t) (print-sketch e))]
    [(Assign var val) (format "MultiType ~a = ~a;" (print-sketch var) (print-sketch val))]
    [(cons s0 s1) (string-append (print-sketch s0) (print-sketch s1))]
    [(? boolean?) (if s 1 0)]
    [(or (? integer?) (? symbol?)) s]))

(define (print-sketch-function f)
  (format "MultiType ~a(~a) { ~a }"
          (FunSK-name f) (string-join (map (lambda (arg) (format "MultiType ~a" arg)) (FunSK-params f)) ",") (print-sketch (FunSK-body f))))
  

(translate/Choices (Choices '(1 2 3)) #t)
    