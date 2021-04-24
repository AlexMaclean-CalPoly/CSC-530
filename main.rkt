#lang typed/racket

(require "parser.rkt" "logic.rkt" "get-invariants.rkt" "control-graph.rkt")

(define (top-verify [s : Sexp]) : Void
  (define second-order-invariants (get-invariants (make-cfg (parse/Program s))))
  (displayln (logic-str* second-order-invariants)))

(define (verify-cmd [args : (Vectorof String)]) : Void
  (match args
    [(vector input) (call-with-file (string->path input) top-verify)]
    [other (error "Invalid arguments")]))

(define (call-with-file [infile : Path] [f : (Sexp -> Void)]) : Void
  (define exp : Sexp (with-input-from-file infile (λ () (cast (read) Sexp))))
  (top-verify exp))

(module+ main
  (verify-cmd (current-command-line-arguments)))