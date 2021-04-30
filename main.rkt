#lang typed/racket/no-check

(require "parser.rkt" "logic.rkt" "get-constraints.rkt"
         "control-graph.rkt" "solver.rkt" "make-invariant.rkt"
         "types.rkt")

(define verbose-mode : (Parameterof Boolean) (make-parameter #f))

(define (top-verify [s : Sexp] [clauses : Integer] [sub-clauses : Integer]) : Void
  (define program (parse s))
  (define constraints (get-constraints (make-cfg program)))
  (define invariant (make-invariant clauses sub-clauses (extract-vars program)))
  (define constraints-1st-order
    (map (λ ([i : Logic]) (simplify (subst-invariant invariant (InvariantL 'I) i)))
                                   constraints))

  (when (verbose-mode)
    (printf "\nSecond Order Constraints:\n~a\n" (logic-str* constraints))
    (printf "\nHypothesized Invariant:\n~a\n" (logic-str invariant))
    (printf "\nFirst Order Constraints:\n~a\n" (logic-str* constraints-1st-order))))

(module+ main

  (define (cmd-integer? [arg : Any]) : Boolean
    (and (string? arg) (string->number arg) (exact-integer? (string->number arg))))
  
  (command-line
   #:program "constraint solving verifier"
   #:once-each
   [("-v" "--verbose") "Verify with verbose messages" (verbose-mode #t)]
   #:args (filepath ands ors)
   (begin
     (unless (and (cmd-integer? ands) (cmd-integer? ors))
       (error "Conjunuction and disjunction counts must be integers got: ~e, ~e" ands ors))
     (top-verify (cast (read (open-input-file (cast filepath String))) Sexp)
                 (cast (string->number ands) Integer) (cast (string->number ors) Integer)))))
