#lang racket

(require "types.rkt" "sketch.rkt" "rewrite.rkt" "parse.rkt" "print-sketch.rkt")

(define (auto-grade student-solution instructor-solution error-model)
  (define rewritten (rewrite student-solution error-model))
  (define student-sketch (translate-sketch rewritten 'student))
  (define teacher-sketch (translate-sketch instructor-solution 'teacher))

  (displayln (complete-sketch student-sketch teacher-sketch)))

(module+ main
  (command-line
   #:program "sketch-autograder"
   #:args (student teacher error-model)
   (auto-grade
    (parse-program (read (open-input-file student)))
    (parse-program (read (open-input-file teacher)))
    (parse-error-model (read (open-input-file error-model))))))