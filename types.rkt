#lang typed/racket

(provide (all-defined-out))

(define-type op-a (U '+ '* '- '/ '**))

(struct (A O) Arith ([a0 : A] [op : O] [a1 : A]) #:prefab)

(define-type Arith-Expr (U Integer (Arith Arith-Expr op-a) Symbol))

(struct (A) Choices ([s : (Listof A)]) #:prefab)

(define-type set-op (U op-a (Choices set-op)))

(define-type Arith-Set-Expr (U Arith-Expr (Arith Arith-Set-Expr set-op) (Choices Arith-Set-Expr)))

(struct Rewrite-Rule ([before : Arith-Expr] [after : Arith-Set-Expr]) #:prefab)

(define-type Error-Model (Listof Rewrite-Rule))




