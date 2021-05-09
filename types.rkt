#lang typed/racket

(provide (all-defined-out))

;; normal mPy
(define-type ArithExpr (ArithExpr* Nothing))
(define-type (ArithExpr* P) (U P Integer Symbol (BinOp (ArithExpr* P) ArithOp)))
(define-type ArithOp (U '+ '* '- '/ '**))

;; rewritten ~mPy~
(define-type ArithSetExpr (ArithSetExpr* Nothing))
(define-type (ArithSetExpr* P) (U Integer Symbol (BinOp (ArithSetExpr* P) ArithSetOp)
                                (Choices (ArithSetExpr* P)) P))
(define-type ArithSetOp (U ArithOp (Choices ArithSetOp)))

(struct (A) Choices ([clauses : (Listof A)]) #:prefab)

;; shared
(struct (A O) BinOp ([a0 : A] [op : O] [a1 : A]) #:prefab)

;; Error Model
(define-type Error-Model (Listof Rewrite-Rule))
(struct Rewrite-Rule ([before : ArithExpr-Rule] [after : ArithSetExpr-Rule]) #:prefab)
(define-type ArithSetExpr-Rule (ArithSetExpr* Subst))
(define-type ArithExpr-Rule (ArithExpr* Subst))
(struct Subst ([s : Symbol]) #:prefab)

;; Sketch
; prefab causes out of memory error on IfSK
(struct IfSK ([then : (Listof StmtSK)] [else : (Listof StmtSK)]) #:transparent)
(struct ReturnSK ([val : ExprSK]) #:transparent)
(define-type StmtSK (U IfSK ReturnSK))
(define-type FunSK (Listof StmtSK))
(define-type ExprSK (U Integer Symbol (BinOp ExprSK ArithOp)))
