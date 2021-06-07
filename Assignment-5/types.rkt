#lang racket

(provide (all-defined-out))

(struct Choices (clauses) #:prefab)

;; General AST
(struct BinOp (a0 op a1) #:prefab)
(struct Op (op) #:prefab)
(struct Not (a) #:prefab)
(struct Assign (name value) #:prefab)
(struct Return (value) #:prefab)
(struct If (test then else) #:prefab)

;; Sketch
(struct Sketch (vars funs) #:prefab)
(struct FunSK (name params body) #:prefab)
(struct CallSK (name params) #:prefab)
(struct Assign-Weight (var) #:prefab)

;; Error Model
(struct Rewrite-Rule (before after) #:prefab)
(struct Subst (s) #:prefab)

