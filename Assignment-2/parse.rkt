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

(provide parse ExprLC (struct-out LamLC) (struct-out AppLC) (struct-out PrimLC))

(define-type ExprLC (U Real Symbol LamLC AppLC PrimLC))
(struct LamLC ([arg : Symbol] [body : ExprLC]) #:transparent)
(struct AppLC ([f : ExprLC] [arg : ExprLC]) #:transparent)
(struct PrimLC ([name : Symbol] [args : (Listof ExprLC)]) #:transparent)

;; Given a function represented as an S-expression, translates it into an AST representation
(define (parse [s : Sexp]) : ExprLC
  (match s
    [(or (? real?) (? symbol?)) s]
    [`(lam ,(? symbol? id) ,body) (LamLC id (parse body))]
    [`(,(or 'ifleq0 '+ '* 'println) ,args ...) (PrimLC (car s) (map parse (cast args (Listof Sexp))))]
    [`(,f ,a) (AppLC (parse f) (parse a))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse '(+ 4 (* 5 6))) (PrimLC '+ (list 4 (PrimLC '* '(5 6)))))
  (check-equal? (parse '((lam a (+ a 1)) 3)) (AppLC (LamLC 'a (PrimLC '+ '(a 1))) 3)))
