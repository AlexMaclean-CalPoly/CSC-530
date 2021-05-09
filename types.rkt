#lang typed/racket/no-check

(provide (all-defined-out))

;; normal mPy
(define-type ArithExpr (ArithExpr* Nothing))
(define-type (ArithExpr* P) (U P Integer Symbol (BinOp (ArithExpr* P) ArithOp)))
(define-type ArithOp (U '+ '* '- '/ '**))
(define-type BoolOp (U 'and 'or))
(define-type CompOp (U '> '== '>=))
(define-type BoolExpr (BoolExpr* Nothing))
(define-type (BoolExpr* P) (U Boolean P (Not (BoolExpr* P)) (BinOp (BoolExpr* P) BoolOp) (BinOp (ArithExpr* P) CompOp)))

(define-type StmtExpr (StmtExpr* Nothing))
(define-type (StmtExpr* P) (U P (Assign (ArithExpr* P)) (Return (ArithExpr* P)) (Pairof (StmtExpr* P) (StmtExpr* P))
                              (If (ArithExpr* P) (BoolExpr* P))))

(define-type (Expr P) (U P Integer Symbol Boolean (BinOp (Expr P) (U ArithOp BoolOp CompOp))
                         (Not (Expr P)) (If (Expr P) (Expr P)) (Return (Expr P))))


;; rewritten ~mPy~
(define-type ArithSetExpr (ArithSetExpr* Nothing))
(define-type (ArithSetExpr* P) (U Integer Symbol (BinOp (ArithSetExpr* P) ArithSetOp)
                                (Choices (ArithSetExpr* P)) P))
(define-type ArithSetOp (U ArithOp (Choices ArithSetOp)))
(define-type CompSetOp (U CompOp (Choices CompSetOp)))
(define-type BoolSetOp (U BoolOp (Choices BoolSetOp)))
(define-type BoolSetExpr (BoolSetExpr* Nothing))
(define-type (BoolSetExpr* P) (U Boolean Symbol (BinOp (BoolSetExpr* P) BoolSetOp) (Choices (BoolSetExpr* P))
                                 (Not (BoolSetExpr* P)) (BinOp (ArithSetExpr* P) CompSetOp)))


(struct (A) Choices ([clauses : (Listof A)]) #:prefab)

;; shared
(struct (A O) BinOp ([a0 : A] [op : O] [a1 : A]) #:prefab)
(struct (A) Not ([a : A]) #:prefab)
(struct (A) Assign ([name : Symbol] [value : A]) #:prefab)
(struct (A) Return ([value : A]) #:prefab)
(struct (A B) If ([test : B] [then : A] [else : A]) #:prefab)
(struct FunSK ([name : Symbol] [params : (Listof Symbol)] [body : Stmt]) #:prefab)
(struct CallSK ([name : Symbol] [params : (Listof Symbol)]) #:prefab)

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
(define-type ExprSK (U Integer Symbol (BinOp ExprSK ArithOp)))

