#lang racket

;; Abstract Version Space Algebra
(provide (all-defined-out))

;; Union of version spaces with same input and output types
(struct UnionVS (spaces) #:transparent)
;; Transformation of another version space, ti and to transform the input and output to the types of
;; the underlying version space. to-1 maps an output from the underlying vs to an output of this type
(struct TransformVS (space ti to to-) #:transparent)
;; Okay, this one is just a backdoor class.
(struct AtomicVS (context update execute) #:transparent)
;; Join of several other version spaces ti breaks the input value down to a list of input values
;; corresponding to the spaces, to breaks an output down into values that correspond to output values.
;; to-1 builds output value types form the joined spaces into an output for the version space
(struct JoinVS (spaces ti to to-) #:transparent)

;; Given a version space and an expected input and output from the version space, returns a new
;; version space consistent with i and o
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
    [(TransformVS space ti _ to-) (map (lambda (o) (to- o i)) (execute space (ti i)))]
    [(JoinVS spaces ti _ to-) (map (lambda (o) (to- o i)) (apply cartesian-product (map execute spaces (ti i))))]
    [(AtomicVS context _ execute) (execute context i)]))
