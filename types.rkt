#lang typed/racket

(provide (all-defined-out))

;; normal mPy
(define-type ArithExpr (U Integer Symbol (BinOp ArithExpr ArithOp)))
(define-type ArithOp (U '+ '* '- '/ '**))

;; rewritten ~mPy~
(define-type ArithExpr~ (U Integer Symbol (BinOp ArithExpr~ ArithOp~) (Choices ArithExpr~)))
(define-type ArithOp~ (U ArithOp (Choices ArithOp~)))

(struct (A) Choices ([s : (Listof A)]) #:prefab)

;; shared
(struct (A O) BinOp ([a0 : A] [op : O] [a1 : A]) #:prefab)

;; Error Model
(define-type Error-Model (Listof Rewrite-Rule))
(struct Rewrite-Rule ([before : ArithExpr] [after : ArithExpr~]) #:prefab)
