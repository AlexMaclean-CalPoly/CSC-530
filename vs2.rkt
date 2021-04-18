#lang racket

;; Abstract Version Space Algebra --------------------------------------------------------------------

;; Union of version spaces with same input and output types
(struct UnionVS (spaces) #:transparent)
;; Transformation of another version space, ti and to transform the input and output to the types of
;; the underlying version space. to-1 maps an output from the underlying vs to an output of this type
(struct TransformVS (space ti to to-) #:transparent)
;; Okay, this one is just a backdoor class.
(struct AtomicVS (context update execute) #:transparent)
;; Join of several other version spaces ti breaks the input value down to a list of input values
;; corresponding to the spaces, to breaks an output down into values that corespond to output values.
;; to-1 builds output value types form the joined spaces into an output for the version space
(struct JoinVS (spaces ti to to-) #:transparent)

;; Given a version space and an expected input and output from the version space, returns a new
;; version space consisten with i and o
(define (update vs i o)
  (match vs
    [(UnionVS spaces) (UnionVS (map (λ (vs) (update vs i o)) spaces))]
    [(TransformVS space ti to to-) (TransformVS (update space (ti i) (to o)) ti to to-)]
    [(JoinVS spaces ti to to-) (JoinVS (map update spaces (ti i) (to o)) ti to to-)]
    [(AtomicVS context update execute) (AtomicVS (update context i o) update execute)]))

;; Returns a set of all possible output values in the version space given the input i
(define (execute vs i)
  (match vs
    [(UnionVS spaces) (apply set-union (map (λ (s) (execute s i)) spaces))]
    [(TransformVS space ti _ to-) (map to- (execute space (ti i)))]
    [(JoinVS spaces ti _ to-) (map to- (apply cartesian-product (map execute spaces (ti i))))]
    [(AtomicVS context _ execute) (execute context i)]))

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

;; ---------------------------------------------------------------------------------------------------

(struct Document (text cursor) #:transparent)

(define Const (basic-AtomicVS (λ (i o) o) (λ (context i) context)))
(define LinearInt (basic-AtomicVS (λ (i o) (- o i)) +))

(define AbsRow (TransformVS Const identity identity identity))
(define RelRow (TransformVS LinearInt identity identity identity))
(define Row (UnionVS (list AbsRow RelRow)))

(define AbsCol (TransformVS Const identity identity identity))
(define RelCol (TransformVS LinearInt identity identity identity))
(define Col (UnionVS (list AbsCol RelCol)))

(define RowCol (JoinVS (list Row Col)
                       (λ (i) (list (car (Document-cursor i)) (cdr (Document-cursor i))))
                       (λ (o) (list (car o) (cdr o)))
                       (λ (o) (cons (first o) (second o)))))

(define ConstStr (basic-AtomicVS (lambda (i o) o) (lambda (context i) context)))

(define Identity (singleton-AtomicVS = identity))

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
  (check-set-equal? (execute Identity 5) '(5)))
