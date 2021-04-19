#lang racket

(require "algebra.rkt")

;; ---------------------------------------------------------------------------------------------------

(define (basic-AtomicVS extract-context apply-function)
  (AtomicVS (void)
            (λ (context i o) (intersect* context (list (extract-context i o))))
            (λ (context i) (if (void? context) (error "Unrestricted Version Space")
                               (map (λ (c) (apply-function c i)) context)))))

(define (singleton-AtomicVS validate execute)
  (AtomicVS #t (lambda (context i o) (and context (validate i o)))
            (lambda (context i) (if context (list (execute i)) '()))))

(define (intersect* a b)
  (if (void? a) b (set-intersect a b)))

(define (to-offset doc)
  (define cursor (Document-cursor doc))
  (apply + (cdr cursor) (map string-length (take (string-split (Document-text doc) "\n") (car cursor)))))

(define (from-offset text offset)
  (define lines (string-split (substring text 0 offset) "\n"))
  (cons (sub1 (length lines)) (string-length (last lines))))



;; ---------------------------------------------------------------------------------------------------

(struct Document (text cursor) #:transparent)

(define Const (basic-AtomicVS (λ (i o) o) (λ (context i) context)))
(define LinearInt (basic-AtomicVS (λ (i o) (- o i)) +))

(define AbsRow (TransformVS Const identity identity (lambda (o i) o)))
(define RelRow (TransformVS LinearInt identity identity (lambda (o i) o)))
(define Row (UnionVS (list AbsRow RelRow)))

(define AbsCol (TransformVS Const identity identity (lambda (o i) o)))
(define RelCol (TransformVS LinearInt identity identity (lambda (o i) o)))
(define Col (UnionVS (list AbsCol RelCol)))

(define RowCol (JoinVS (list Row Col)
                       (λ (i) (list (car (Document-cursor i)) (cdr (Document-cursor i))))
                       (λ (o) (list (car o) (cdr o)))
                       (λ (o) (cons (first o) (second o)))))

(define ConstStr (basic-AtomicVS (lambda (i o) o) (lambda (context i) context)))

(define Identity (singleton-AtomicVS = identity))

(define CharOffset (TransformVS LinearInt to-offset to-offset (lambda (o i) (from-offset o))))

(define Location (UnionVS (list RowCol CharOffset)))

(define Move (TransformVS Location identity Document-cursor (lambda (o i) (Document (Document-text i) o))))

(define Action (UnionVS (list Move)))
;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define-simple-check (check-set-equal? a e) (equal? (list->set e) (list->set a)))

  (define c (update Const (void) 5))
  (check-set-equal? (execute c (void)) '(5))
  (check-set-equal? (execute (update c (void) 7) (void)) '())
  (check-exn #rx"Unrestricted Version Space" (thunk (execute Const (void))))

  (define lin (update LinearInt 2 4))
  (check-set-equal? (execute lin 5) '(7))
  (check-set-equal? (execute (update c 0 7) 3) '())

  (check-set-equal? (execute (update AbsRow (void) 5) (void)) '(5))

  (define row (update Row 0 3))
  (check-set-equal? (execute row 5) '(3 8))
  (check-set-equal? (execute row -1) '(3 2))
  (check-set-equal? (execute (update row 1 4) 7) '(10))

  (define rc (update RowCol (Document "a" '(1 . 1)) '(1 . 2)))
  (check-set-equal? (execute rc (Document "b" '(1 . 1))) '((1 . 2)))
  (check-set-equal? (execute rc (Document "b" '(3 . 4)))
                    '((3 . 5) (3 . 2) (1 . 5) (1 . 2)))
  (check-set-equal? (execute (update rc (Document "b" '(1 . 2)) '(1 . 3)) (Document "b" '(4 . 6)))
                    '((1 . 7) (4 . 7)))
  (check-set-equal? (execute Identity 5) '(5))

  (check-equal? (to-offset (Document "hello\nworld" '(1 . 2))) 7)
  (check-equal? (from-offset "hello\nworld" 7) '(1 . 2)))
