#lang typed/racket/no-check

(require "parser.rkt" "logic.rkt" "get-invariants.rkt" "control-graph.rkt" "solver.rkt")

(define (top-verify [s : Sexp]) : Void
  (define program (parse/Program s))
  (define second-order-invariants (get-invariants (make-cfg program)))
  (displayln (logic-str* second-order-invariants))
  (define I (simplify (make-I 2 2 (extract-vars program))))
  (displayln (logic-str I))
  (define first-order-invariants (map (lambda ([i : Logic]) (simplify (subst I 'I i))) second-order-invariants))
  (displayln (logic-str* first-order-invariants))
  (void))

(define (verify-cmd [args : (Vectorof String)]) : Void
  (match args
    [(vector input) (call-with-file (string->path input) top-verify)]
    [other (error "Invalid arguments")]))

(define (call-with-file [infile : Path] [f : (Sexp -> Void)]) : Void
  (define exp : Sexp (with-input-from-file infile (λ () (cast (read) Sexp))))
  (top-verify exp))

(module+ main
  (verify-cmd (current-command-line-arguments)))