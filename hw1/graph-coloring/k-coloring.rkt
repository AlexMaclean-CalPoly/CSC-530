#lang racket

(require "solver.rkt" "graph.rkt")

(provide
 k-coloring      ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
 valid-coloring? ; (-> graph/c coloring/c boolean?)
 )

; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (and (coloring/c coloring)
       (= (color-size coloring) (node-count graph))
       (for*/and ([(e n) (in-indexed graph)] [child e])
         (not (= (color-ref coloring n) (color-ref coloring child))))))

; Returns a coloring/c if the given graph can 
; be colored with k colors.  Otherwise returns #f.
(define (k-coloring graph k)
  (define v (vector-length graph))
  (define vertex-constraint (build-list v (λ (index) (build-list k (λ (c) (+ c (* k index) 1))))))
  (define edge-constraint (append-map (λ (w edges)
                                        (append-map (λ (v)
                                                      (build-list k (λ (c)
                                                                      (list (- (+ c (* k v) 1))
                                                                            (- (+ c (* k w) 1))))))
                                                    edges))
                                      (range v) (vector->list graph)))
  (define solution (solve (append vertex-constraint edge-constraint)))
  (displayln solution))
