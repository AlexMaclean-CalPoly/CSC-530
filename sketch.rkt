#lang racket

(provide (rename-out [top-translate translate-sketch]))

(require "types.rkt")

;;
(define (top-translate mpy name)
  (define funs (make-hash))
  (define main (translate mpy (make-hash) funs))
  (Sketch (filter (negate FunSK?) (hash-values funs))
          (cons (FunSK name '() main) (filter FunSK? (hash-values funs)))))

;; 
(define (translate a env funs)
  (match a
    [(or (? integer?) (? symbol?) (? boolean?) (? Op?)) a]
    [(BinOp a op b) (BinOp (translate a env funs) (translate op env funs) (translate b env funs))]
    [(Assign var val)
     (let ([asgn (Assign (translate var env funs) (translate val env funs))])
       (hash-set! env var #t) asgn)]
    [(Return val) (Return (translate val env funs))]
    [(cons s0 s1) (cons (translate s0 env funs) (translate s1 env funs))]
    [(Choices (list 1st)) (translate 1st env funs)]
    [(Choices (list c ..2))
     (let* [(name (gensym 'choices))
            (bool (gensym 'choice))
            (params (hash-keys env))
            (body (translate/Choices c env funs bool))]
       (hash-set! funs name (FunSK name params body))
       (hash-set! funs bool bool)
       (CallSK name params))]))

;;
(define (translate/Choices choices env funs bool)
  (match choices
    [(list 1st 2nd)
     (If '?? (Return (translate 1st env funs))
         (cons (Assign-Weight bool) (Return (translate 2nd env funs))))]
    [(list 1st rst ..1)
     (If '?? (Return (translate 1st env funs))
         (cons (Assign-Weight bool) (translate/Choices rst env funs bool)))]))
