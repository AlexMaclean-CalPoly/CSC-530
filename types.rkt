#lang typed/racket

(provide (all-defined-out))

;; normal mPy
(define-type ArithExpr (ArithExpr* Nothing))
(define-type (ArithExpr* P) (U P Integer Symbol (BinOp ArithExpr ArithOp)))
(define-type ArithOp (U '+ '* '- '/ '**))

;; rewritten ~mPy~
(define-type ArithExpr~ (ArithExpr~* Nothing))
(define-type (ArithExpr~* P) (U Integer Symbol (BinOp ArithExpr~ ArithOp~) (Choices ArithExpr~) P))
(define-type ArithOp~ (U ArithOp (Choices ArithOp~)))

(struct (A) Choices ([clauses : (Listof A)]) #:prefab)

;; shared
(struct (A O) BinOp ([a0 : A] [op : O] [a1 : A]) #:prefab)

;; Error Model
(struct Subst ([s : Symbol]) #:prefab)
(define-type Error-Model (Listof Rewrite-Rule))
(struct Rewrite-Rule ([before :  (ArithExpr* Subst)] [after : (ArithExpr~* Subst)]) #:prefab)

;; Sketch
; prefab causes out of memory error on IfSK
(struct IfSK ([then : (Listof StmtSK)] [else : (Listof StmtSK)]) #:transparent)
(struct ReturnSK ([val : ExprSK]) #:transparent)
(define-type StmtSK (U IfSK ReturnSK))
(define-type FunSK (Listof StmtSK))
(define-type ExprSK (U Integer Symbol (BinOp ExprSK ArithOp)))
