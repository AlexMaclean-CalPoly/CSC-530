#lang typed/racket
(provide (all-defined-out))

#|

Program = (Stmt ...)

Stmt = (id := Exp)
     | (id := ?)
     | (while Exp (Stmt ...))
     | (assert Exp)
     | (assume Exp)
     | (if Exp (Stmt ...))

Exp = id
    | num
    | (Exp Prim Exp)

Prim = + | * | - | / | < | > | =

|#

(define-type Program (Listof Stmt))

(define-type Stmt (U While If Assert Assume Assign))
(struct While ([test : Exp] [body : Program]) #:transparent)
(struct If ([test : Exp] [body : Program]) #:transparent)
(struct Assert ([test : Exp]) #:transparent)
(struct Assume ([test : Exp]) #:transparent)
(struct Assign ([var : Symbol] [val : Exp]) #:transparent)

(define-type Exp (U Integer Symbol Prim))
(struct Prim ([op : Symbol] [vars : (Listof Exp)]) #:transparent)

;; ---------------------------------------------------------------------------------------------------

(define (parse/Program [s : Sexp]) : Program
  (if (list? s) (map parse/Stmt s) (error 'parse "Invalid Program ~e" s)))

(define (parse/Stmt [s : Sexp]) : Stmt
  (match s
    [`(,(? symbol? id) := ?) (Assign id (gensym 'r))]
    [`(,(? symbol? id) := ,exp) (Assign id (parse/Exp exp))]
    [`(while ,test ,(? list? body)) (While (parse/Exp test) (parse/Program body))]
    [`(if ,test ,(? list? body)) (If (parse/Exp test) (parse/Program body))]
    [`(assert ,e) (Assert (parse/Exp e))]
    [`(assume ,e) (Assume (parse/Exp e))]
    [_ (error 'parse "Invalid Stmt ~e" s)]))

(define (parse/Exp [s : Sexp]) : Exp
  (match s
    [(or (? exact-integer?) (? symbol?)) s]
    [`(,a ,(? prim? op) ,b) (Prim (cast op Symbol) (list (parse/Exp a) (parse/Exp b)))]
    [_ (error 'prase "Invalid Exp ~e" s)]))

(define (prim? [s : Any]) : Boolean
  (list? (member s '(+ - * / > < = <= >=))))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse/Program
                 '{{x := -50}
                   {while (x < 0)
                          {
                           {x := (x + y)}
                           {y := (y + 1)}
                           }}
                   {assert(y > 0)}})
                (list
                 (Assign 'x -50)
                 (While (Prim '< '(x 0))
                        (list
                         (Assign 'x (Prim '+ '(x y)))
                         (Assign 'y (Prim '+ '(y 1)))))
                 (Assert (Prim '> '(y 0))))))
