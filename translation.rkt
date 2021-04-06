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

(define-type ExprC (U Real Symbol LamC AppC PrimC))
(struct LamC ([arg : Symbol] [body : ExprC]) #:transparent)
(struct AppC ([f : ExprC] [arg : ExprC]) #:transparent)
(struct PrimC ([name : Symbol] [args : (Listof ExprC)]) #:transparent)

;; Given a function represented as an S-expression, translates it into an AST representation
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) s]
    [(? symbol?) s]
    [`(/ ,(? symbol? id) => ,body) (LamC id (parse body))]
    [`(,(? symbol? name) ,args ...) (PrimC name (map parse (cast args (Listof Sexp))))]
    [`(,f ,a) (AppC (parse f) (parse a))]))

;; Translates the AST back into a string representing equivalent python code
(define (python [e : ExprC]) : String
  (match e
    [(? real? r) (~v r)]
    [(? symbol? id) (~a id)]
    [(LamC arg body) (format "(lambda ~a: ~a)" arg (python body))]
    [(AppC f arg) (format "(~a(~a))" (python f) (python arg))]
    [(PrimC '+ (list a b)) (format "(~a + ~a)" (python a) (python b))]
    [(PrimC '* (list a b)) (format "(~a * ~a)" (python a) (python b))]
    [(PrimC 'ifleq0 (list f t e))
     (format "(~a if (~a <= 0) else ~a)" (python t) (python f) (python e))]
    [(PrimC 'println (list e)) (format "print(~a)" (python e))]))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse '(+ 4 (* 5 6))) (PrimC '+ (list 4 (PrimC '* '(5 6)))))
  (check-equal? (parse '((/ a => (+ a 1)) 3)) (AppC (LamC 'a (PrimC '+ '(a 1))) 3))

  (check-equal?
   (python (AppC (LamC 'a (PrimC 'println (list (PrimC '+ '(a 1)))))
                 (PrimC 'ifleq0 (list 1 (PrimC '* '(5 7)) 6))))
   "((lambda a: print((a + 1)))(((5 * 7) if (1 <= 0) else 6)))"))