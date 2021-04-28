#lang typed/racket/no-check

(require "parser.rkt" "logic.rkt" "get-invariants.rkt" "control-graph.rkt" "solver.rkt")

(define verbose-mode (make-parameter #f))

(define (top-verify [s : Sexp] [clauses : Integer] [sub-clauses : Integer]) : Void
  (define program (parse s))
  (define constraints (get-constraints (make-cfg program)))
  (define invariant (make-invariant clauses sub-clauses (extract-vars program)))
  ;(define constraints-1st-order (map (λ ([i : Logic]) (simplify (subst invariant 'I i)))
  ;                                  constraints))

  (when (verbose-mode)
    (printf "\nSecond Order Constraints:\n~a\n" (logic-str* constraints))
    (printf "\nHypothesized Invariant:\n~a\n" (logic-str invariant))))
    ;(printf "\nFirst Order Constraints:\n~a\n" (logic-str* constraints-1st-order))))

(module+ main
  (command-line
   #:program "constraint solving verifier"
   #:once-each
   [("-v" "--verbose") "Verify with verbose messages" (verbose-mode #t)]
   #:args (filepath ands ors)
   (begin
     (unless (and (string->number ands) (string->number ors))
       (error "Conjunuction and disjunction counts must be integers"))
     (top-verify (cast (with-input-from-file filepath read) Sexp)
                 (string->number ands) (string->number ors)))))
