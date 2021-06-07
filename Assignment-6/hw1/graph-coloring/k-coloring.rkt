#lang racket

(require "solver.rkt" "graph.rkt")

(provide
 k-coloring       ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
 valid-coloring?) ; (-> graph/c coloring/c boolean?)


;; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (and (coloring/c coloring)
       (= (color-size coloring) (node-count graph))
       (for*/and ([(e n) (in-indexed graph)] [child e])
         (not (= (color-ref coloring n) (color-ref coloring child))))))

;; Returns a coloring/c if the given graph can
;; be colored with k colors.  Otherwise returns #f.
(define (k-coloring graph k)
  (get-coloring (solve (get-constraints graph k)) k))

;; Returns a cnf/c representing a reduction of the coloring problem represented by graph and k
(define (get-constraints graph k)
  (append
   (for/list ([v (vector-length graph)])
     (for/list ([c k]) (var v c k)))
   (for/list ([v (vector-length graph)] [edges graph] #:when #t [w edges] #:when #t [c k])
     (list (- (var v c k)) (- (var w c k))))))

;; Given a vertex index v, a color c, and the total number of colors k, returns the
;; corresponding literal
(define (var v c k)
  (+ c (* k v) 1))

;; Given an interpretation i, returns a coloring/c of k colors or #f
(define (get-coloring i k)
  (and i (for/vector ([v-offset (range 0 (length i) k)] [colors (split-many i k)])
           (- (findf positive? colors) v-offset 1))))

;; Given a list splits it up into a list of many lists, each with n elements
(define (split-many lst n)
  (if (empty? lst) lst
      (cons (take lst n) (split-many (drop lst n) n))))


