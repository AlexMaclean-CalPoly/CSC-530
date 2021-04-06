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

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) s]
    [(? symbol?) s]
    [`(/ ,(? symbol? id) => ,body) (LamC id (parse body))]
    [`(,(? symbol? name) ,args ...) (PrimC name (map parse (cast args (Listof Sexp))))]
    [`(,f ,a) (AppC (parse f) (parse a))]))


(define (translate [e : ExprC]) : String
  (match e
    [(? real? r) (~v r)]
    [(? symbol? id) (~a id)]
    [(LamC arg body) (format "(lambda ~a: ~a)" arg (translate body))]
    [(AppC f arg) (format "(~a(~a))" (translate f) (translate arg))]
    [(PrimC '+ (list a b)) (format "(~a + ~a)" (translate a) (translate b))]
    [(PrimC '* (list a b)) (format "(~a * ~a)" (translate a) (translate b))]
    [(PrimC 'ifleq0 (list f t e))
     (format "(~a if (~a <= 0) else ~a)" (translate t) (translate f) (translate e))]
    [(PrimC 'println (list e)) (format "print(~a)" (translate e))]))