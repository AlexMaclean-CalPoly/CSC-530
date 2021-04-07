#lang typed/racket
(provide parse extract-funs top-translate)

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

(define-type ExprLC (U Real Symbol LamC AppC PrimC FunC))
(struct LamC ([arg : Symbol] [body : ExprLC]) #:transparent)
(struct AppC ([f : ExprLC] [arg : ExprLC]) #:transparent)
(struct PrimC ([name : Symbol] [args : (Listof ExprLC)]) #:transparent)
(struct FunC ([name : Symbol]) #:transparent)
(struct EnvC ([i : Integer]) #:transparent)


;; Given a function represented as an S-expression, translates it into an AST representation
(define (parse [s : Sexp]) : ExprLC
  (match s
    [(? real?) s]
    [(? symbol?) s]
    [`(lam ,(? symbol? id) ,body) (LamC id (parse body))]
    [`(,(? symbol? name) ,args ...) (PrimC name (map parse (cast args (Listof Sexp))))]
    [`(,f ,a) (AppC (parse f) (parse a))]))


(define (extract-funs [e : ExprLC] [funs : (Mutable-HashTable Symbol ExprLC)] [env : (Listof Symbol)]) : ExprLC
  (match e
    [(? real?) e]
    [(? symbol?) e]
    [(PrimC name args) (PrimC name (map (lambda ([arg : ExprLC]) (extract-funs arg funs env)) args))]
    [(AppC f arg) (AppC (extract-funs f funs env) (extract-funs arg funs env))]
    [(LamC arg body) (add-fun body funs env)]))

(define (uncover-env [e : ExprLC] [env: (Listof Symbol)]) : ExprLC
  (match e
    [(? real?) e]
    [(? symbol?) (index

(define (add-fun [e : ExprLC] [funs : (Mutable-HashTable Symbol ExprLC)] [env : (Listof Symbol)]) : FunC
  (define name (gensym 'lambda))
  (hash-set! funs name (extract-funs e funs env))
  (FunC name))


(define (top-translate [s : Sexp]) : Any
  (define funs : (Mutable-HashTable Symbol ExprLC) (make-hash))
  (define main (extract-funs (parse s) funs '()))
  (string-join (hash-map funs c-translate-fun) "\n\n"))

(define (c-translate [e : ExprLC]) : String
  (match e
    [(? real?) (~v e)]
    [(? symbol?) (~a e)]
    [(PrimC 'println (list arg)) (format "printf(\"%d\\n\", (int) ~a)" (c-translate arg))]
    [(PrimC 'ifleq0 (list f t e)) (format "((int) ~a <= 0 ? ~a : ~a)" (c-translate f) (c-translate t) (c-translate e))]
    [(PrimC '+ (list a b)) (format "((int) ~a + (int) ~a)" a b)]
    [(PrimC '* (list a b)) (format "((int) ~a * (int) ~a)" a b)]
    [(AppC name arg) ] ))

(define (c-translate-fun [s : Symbol] [e : ExprLC]) : String
  (format "void* ~a(void* env) {
    return ~a;
}" s (c-translate e)))

(displayln (top-translate 5))
(displayln (top-translate '(lam x (+ x 1))))
(displayln (top-translate '(lam x (+ x ((lam y (ifleq0 y 1 -1)) 0)))))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (check-equal? (parse '(+ 4 (* 5 6))) (PrimC '+ (list 4 (PrimC '* '(5 6)))))
  (check-equal? (parse '((/ a => (+ a 1)) 3)) (AppC (LamC 'a (PrimC '+ '(a 1))) 3)))

