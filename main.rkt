#lang typed/racket/no-check

(require "parse.rkt" "logic.rkt" "get-constraints.rkt" "control-graph.rkt" "simplify.rkt"
         "make-invariant.rkt" "types.rkt" "farkas.rkt" "z3.rkt")

(define verbose-mode : (Parameterof Boolean) (make-parameter #f))
(define sexp-mode : (Parameterof Boolean) (make-parameter #f))

(define (top-verify [s : Sexp] [clauses : Integer] [sub-clauses : Integer]) : Void
  (define program (parse s))
  (define constraints (get-constraints (make-cfg program)))
  (define invariant (make-invariant clauses sub-clauses (extract-vars program)))
  (define constraints-1st-order
    (map (λ ([i : Logic])
           (simplify (subst-invariant invariant (InvariantL 'I) i))) constraints))
  (define farkas (apply-farkas constraints-1st-order))

  (when (verbose-mode)
    (console-display "Second Order Constraints" constraints)
    (console-display "Hypothesized Invariant" invariant)
    (console-display "First Order Constraints" constraints-1st-order))

  (map displayln (to-z3 farkas)))

(define (console-display [title : String] [data : (U Logic (Listof Logic))]) : Void
  (define disp (if (sexp-mode) logic->sexp logic->string))
  (printf "\n~a:\n~a\n" title (if (list? data) (map disp data) (disp data))))

(module+ main

  (define (cmd-integer? [arg : Any]) : Boolean
    (and (string? arg) (string->number arg) (exact-integer? (string->number arg))))

  (command-line
   #:program "constraint solving verifier"
   #:once-each
   [("-v" "--verbose") "Verify with verbose messages" (verbose-mode #t)]
   #:once-any
   [("-x" "--sexp") "Display output as prefix s-expressions" (sexp-mode #t)]
   [("-s" "--string") "Display output as infix strings" (sexp-mode #f)]
   #:args (filepath ands ors)
   (begin
     (unless (and (cmd-integer? ands) (cmd-integer? ors))
       (error "Conjunuction and disjunction counts must be integers got: ~e, ~e" ands ors))
     (top-verify (cast (read (open-input-file (cast filepath String))) Sexp)
                 (cast (string->number ands) Integer) (cast (string->number ors) Integer)))))
