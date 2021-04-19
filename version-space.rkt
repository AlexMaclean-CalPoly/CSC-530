#lang racket

(require "algebra.rkt" "document.rkt")


(define (basic-AtomicVS extract-context apply-function)
  (AtomicVS (void)
            (λ (context i o) (intersect* context (list (extract-context i o))))
            (λ (context i) (if (void? context) (error "Unrestricted Version Space")
                               (map (λ (c) (apply-function c i)) context)))))

(define (singleton-AtomicVS validate execute)
  (AtomicVS #t (λ (context i o) (and context (validate i o)))
            (λ (context i) (if context (list (execute i)) '()))))

(define (intersect* a b)
  (if (void? a) b (set-intersect a b)))

;; ---------------------------------------------------------------------------------------------------

(define Const (basic-AtomicVS (λ (i o) o) (λ (context i) context)))
(define ConstStr (basic-AtomicVS (λ (i o) o) (λ (context i) context)))
(define LinearInt (basic-AtomicVS (λ (i o) (- o i)) +))
(define Identity (singleton-AtomicVS = identity))

(define AbsRow (TransformVS* Const identity identity identity))
(define RelRow (TransformVS* LinearInt identity identity identity))
(define Row (UnionVS (list AbsRow RelRow)))

(define AbsCol (TransformVS* Const identity identity identity))
(define RelCol (TransformVS* LinearInt identity identity identity))
(define Col (UnionVS (list AbsCol RelCol)))

(define RowCol (JoinVS* (list Row Col)
                        (λ (i) (list (car (Document-cursor i)) (cdr (Document-cursor i))))
                        (λ (o) (list (car o) (cdr o)))
                        (λ (o) (cons (first o) (second o)))))

(define CharOffset (TransformVS LinearInt
                                (λ (i) (loc->offset (Document-text i) (Document-cursor i)))
                                (λ (o i) (loc->offset (Document-text i) o))
                                (λ (o i) (offset->loc (Document-text i) o))))

(define Location (UnionVS (list RowCol CharOffset)))

(define Move (TransformVS Location identity
                          (λ (o i) (if (equal? (Document-text i) (Document-text o))
                                       (Document-cursor o) (void)))
                          (λ (o i) (Document (Document-text i) o))))

(define Delete (JoinVS (list Location Location)
                       (λ (i) (list i i))
                       (λ (o i) (or (and (equal? (Document-cursor i) (Document-cursor o))
                                         (deleted-locs (Document-text i) (Document-text o)))
                                    (list (void) (void))))
                       (λ (o i) (apply delete-text i o))))

(define Insert (TransformVS (UnionVS (list ConstStr)) identity
                            (λ (o i) (or (get-inserted i o) (void)))
                            (λ (o i) (insert-text i o))))

(define Action (UnionVS (list Move Delete Insert)))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define-simple-check (check-set-equal? a e) (equal? (list->set e) (list->set a)))

  (define c (update Const (void) 5))
  (check-set-equal? (execute c (void)) '(5))
  (check-set-equal? (execute (update c (void) 7) (void)) '())
  (check-set-equal? (execute (update Const (void) (void)) (void)) '())
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

  (define a (update Action (Document "abc\ndef" '(0 . 2)) (Document "abc\ndef" '(1 . 0))))
  (check-set-equal? (execute a (Document "123456\nabcdefg\nhijklmnop" '(1 . 1)))
                    (list (Document "123456\nabcdefg\nhijklmnop" '(1 . 3))
                          (Document "123456\nabcdefg\nhijklmnop" '(2 . -1))
                          (Document "123456\nabcdefg\nhijklmnop" '(2 . 0))
                          (Document "123456\nabcdefg\nhijklmnop" '(1 . -1))
                          (Document "123456\nabcdefg\nhijklmnop" '(1 . 0))))
  (check-equal? (execute (update Move (Document "abc" '(1 . 2)) (Document "def" '(3 . 4)))
                         (Document "ac" '(0 . 0)))
                '())

  (define del-line1 (update (update Action (Document "abc\ndef" '(0 . 0)) (Document "def" '(0 . 0)))
                      (Document "line 1\nline 2" '(1 . 1)) (Document "line 2" '(1 . 1))))
  (check-set-equal? (execute del-line1 (Document "123456\nabcdefg\nhijklmnop" '(1 . 1)))
                    (list (Document "abcdefg\nhijklmnop" '(1 . 1))))

  (define del-next (update (update Action
                                     (Document "this is a string" '(0 . 3))
                                     (Document "thi is a string" '(0 . 3)))
                      (Document "tringing is\nimportant" '(1 . 2))
                      (Document "tringing is\nimrtant" '(1 . 2))))
  (check-set-equal? (execute del-next (Document "--x--" '(0 . 2)))
                    (list (Document "---" '(0 . 2))))

  (check-set-equal? (execute (update Action (Document "abc" '(0 . 1)) (Document "axybc" '(0 . 3)))
                             (Document "hello world" '(0 . 3)))
                    (list (Document "helxylo world" '(0 . 5)))))

  
