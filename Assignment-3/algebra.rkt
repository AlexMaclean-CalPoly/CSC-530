#lang racket

;; Abstract Version Space Algebra
(provide (all-defined-out))

;; Union of version spaces with same input and output types
(struct UnionVS (spaces) #:transparent)
;; Transformation of another version space, ti and to transform the input and output to the types of
;; the underlying version space. to-1 maps an output from the underlying vs to an output of this type
(struct TransformVS (space ti to to-) #:transparent)
;; For atomic version spaces, there are various valid ways to represent them depending on what is
;; inside, we didn't find a one size fits all way to update and execute them, so they can sort
;; of be defined however
(struct AtomicVS (context update execute) #:transparent)
;; Join of several other version spaces ti breaks the input value down to a list of input values
;; corresponding to the spaces, to breaks an output down into values that correspond to output values.
;; to-1 builds output value types form the joined spaces into an output for the version space
(struct JoinVS (spaces ti to to-) #:transparent)

;; There are lots of ways to define this, but this is the easiest
(define EmptyVS (AtomicVS '() (const '()) (const '())))

;; Wrapper for the case when the ouput transformation functions require no info about the input
(define (TransformVS* space ti to to-)
  (TransformVS space ti (λ (o i) (to o)) (λ (o i) (to- o))))

;; Wrapper for the case when the ouput transformation functions require no info about the input
(define (JoinVS* spaces ti to to-)
  (JoinVS spaces ti (λ (o i) (to o)) (λ (o i) (to- o))))

;; Given a version space and an expected input and output from the version space, returns a new
;; version space consistent with i and o. Since all version spaces represent functions returning
;; some value, updating a version space with void will cause it to become empty
(define (update vs i o)
  (if (void? o) EmptyVS
      (match vs
        [(UnionVS spaces) (UnionVS (map (λ (vs) (update vs i o)) spaces))]
        [(TransformVS space ti to to-) (TransformVS (update space (ti i) (to o i)) ti to to-)]
        [(JoinVS spaces ti to to-) (JoinVS (map update spaces (ti i) (to o i)) ti to to-)]
        [(AtomicVS context update execute) (AtomicVS (update context i o) update execute)])))

;; Returns a set of all possible output values in the version space given the input i
(define (execute vs i)
  (match vs
    [(UnionVS spaces) (apply set-union (map (λ (s) (execute s i)) spaces))]
    [(TransformVS space ti _ to-) (map (λ (o) (to- o i)) (execute space (ti i)))]
    [(JoinVS spaces ti _ to-)
     (map (λ (o) (to- o i)) (apply cartesian-product (map execute spaces (ti i))))]
    [(AtomicVS context _ execute) (execute context i)]))
