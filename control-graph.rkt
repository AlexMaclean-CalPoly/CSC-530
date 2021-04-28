#lang typed/racket

(provide make-cfg)

(require "types.rkt")

(define (make-cfg [p : Program]) : CFG
  (define cfg : CFG (make-hash))
  (hash-set! cfg (Cut-Point 'start) (cfg/block cfg p (Cut-Point 'end)))
  cfg)

(define (cfg/block [cfg : CFG] [p : Program] [next : Label]) : Basic-Block
  (match p
    [(cons (If test body) rst)
     (let ([if-label (gensym 'if)]
           [true-label (gensym 'true)]
           [rest-label (gensym 'rest)])
       (hash-set! cfg if-label (Conditional test true-label rest-label))
       (hash-set! cfg true-label (cfg/block cfg body rest-label))
       (hash-set! cfg rest-label (cfg/block cfg rst next))
       (list (GoTo if-label)))]
    [(cons (While test body) rst)
     (let ([while-label (Cut-Point (gensym 'while))]
           [true-label (gensym 'true)]
           [rest-label (gensym 'rest)])
       (hash-set! cfg while-label (Conditional test true-label rest-label))
       (hash-set! cfg true-label (cfg/block cfg body while-label))
       (hash-set! cfg rest-label (cfg/block cfg rst next))
       (list (GoTo while-label)))]
    [(cons other rst) (cons other (cfg/block cfg rst next))]
    ['() (list (GoTo next))]))

