#lang typed/racket

#|
  LC = num
     | id
     | (/ id => LC)
     | (LC LC)
     | (+ LC LC)
     | (* LC LC)
     | (ifleq0 LC LC LC)
     | (println LC)
|#

(provide parse ExprLC (struct-out LamC) (struct-out AppC) (struct-out PrimC))

(define-type ExprLC (U Real Symbol LamC AppC PrimC))
(struct LamC ([arg : Symbol] [body : ExprLC]) #:transparent)
(struct AppC ([f : ExprLC] [arg : ExprLC]) #:transparent)
(struct PrimC ([name : Symbol] [args : (Listof ExprLC)]) #:transparent)

;; Given a function represented as an S-expression, translates it into an AST representation
(define (parse [s : Sexp]) : ExprLC
  (match s
    [(or (? real?) (? symbol?)) s]
    [`(/ ,(? symbol? id) => ,body) (LamC id (parse body))]
    [`(,(or 'ifleq0 '+ '* 'println) ,args ...) (PrimC (car s) (map parse (cast args (Listof Sexp))))]
    [`(,f ,a) (AppC (parse f) (parse a))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse '(+ 4 (* 5 6))) (PrimC '+ (list 4 (PrimC '* '(5 6)))))
  (check-equal? (parse '((/ a => (+ a 1)) 3)) (AppC (LamC 'a (PrimC '+ '(a 1))) 3)))
