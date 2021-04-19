#lang racket

(provide (all-defined-out))

(require srfi/13)

(struct Document (text cursor) #:transparent)


(define (loc->offset text loc)
  (apply + (cdr loc)
         (map (compose add1 string-length)
              (take (string-split text "\n") (car loc)))))

(define (offset->loc text offset)
  (define lines (string-split (substring text 0 (min offset (string-length text))) "\n"))
  (cons (sub1 (length lines)) (string-length (last lines))))


(module+ test
  (require rackunit)

  (check-equal? (loc->offset (Document "hello\nworld" '(1 . 2))) 8)
  (check-equal? (offset->loc "hello\nworld" 8) '(1 . 2)))

