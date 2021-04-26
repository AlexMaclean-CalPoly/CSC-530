#lang typed/racket/no-check

(require "parser.rkt" "logic.rkt" "get-invariants.rkt" "control-graph.rkt" "solver.rkt")

(define (top-verify [s : Sexp] [clauses : Integer] [sub-clauses : Integer]) : Void
  (define program (parse/Program s))
  (define constraints (get-invariants (make-cfg program)))
  (define invariant (simplify (make-I clauses sub-clauses (extract-vars program))))
  (define constraints-1st-order (map (λ ([i : Logic]) (simplify (subst invariant 'I i)))
                                      constraints))

  (printf "\nSecond Order Constraints:\n~a\n" (logic-str* constraints))
  (printf "\nHypothesized Invariant:\n~a\n" (logic-str invariant))
  (printf "\nFirst Order Constraints:\n~a\n" (logic-str* constraints-1st-order)))

(define (verify-cmd [args : (Vectorof String)]) : Void
  (match args
    [(vector input (? string->number clauses) (? string->number sub-clauses))
     (call-with-file (string->path input)
                     (λ ([s : Sexp]) (top-verify s (string->number clauses)
                                                 (string->number sub-clauses))))]
    [other (error "Invalid arguments")]))

(define (call-with-file [infile : Path] [f : (Sexp -> Void)]) : Void
  (f (cast (with-input-from-file infile read) Sexp)))

(module+ main
  (verify-cmd (current-command-line-arguments)))
