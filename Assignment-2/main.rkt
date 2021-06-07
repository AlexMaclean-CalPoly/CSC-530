#lang typed/racket

(require "c.rkt" "python.rkt")

(define (transpile [args : (Vectorof String)]) : Void
  (match args
    [(vector "c" input) (call-with-files (string->path input) #".c" top-c)]
    [(vector "python" input) (call-with-files (string->path input) #".py" top-python)]
    [other (error "Invalid arguments")]))

(define (call-with-files [infile : Path] [ext : Bytes] [f : (Sexp -> String)]) : Void
  (define exp : Sexp (with-input-from-file infile (λ () (cast (read) Sexp))))
  (with-output-to-file (path-replace-extension infile ext) #:exists 'truncate
    (λ () (display (f exp)))))

(module+ main
  (transpile (current-command-line-arguments)))