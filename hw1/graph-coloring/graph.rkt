#lang racket

(require racket/draw)

(provide 
  graph/c    ; contract? for recognizing graph specifications
  coloring/c ; contract? for recognizing coloring specifications
  node?      ; (-> any/c boolean?) recognizes a node in a graph (a natural number)
  read-graph ; (-> (or/c path? string?) graph/c)
  nodes      ; (-> graph/c stream?)
  edges      ; (-> graph/c stream?)
  children   ; (-> graph/c node? (listof node?))
  node-count ; (-> graph/c natural-number/c)
  edge-count ; (-> graph/c natural-number/c)]
  color-ref  ; (-> coloring/c natural-number/c natural-number/c)
  color-size ; (-> coloring/c natural-number/c)
  dot        ; (case-> (-> path-string?) (-> path-string? void?))
  visualize  ; (->* (graph/c) [(or/c #f coloring/c)] (is-a?/c bitmap%))
)

; We represent a graph as a vector of lists.  In particular, 
; every node in the graph is identified by a natural number in 
; the range [0 ... (vector-length graph)).  The list in the ith 
; position contains the (possibly empty) list of ith node's children 
; in the graph. 
(define node? natural-number/c)
(define graph/c (vector-immutableof (listof node?)))

(define node-count vector-length)

(define (nodes g) ; returns a stream of sorted node ids
  (in-range (node-count g)))

(define (edge-count g) 
  (for/sum ([e g]) (length e)))

(define (edges g) ; returns a stream of edges (pairs of node ids)
  (in-list (apply append (for/list ([(e n) (in-indexed g)])
                           (map (curry cons n) e)))))

(define children vector-ref)

; Reads the given DIMACS file and returns the
; corresponding graph.  
(define (read-graph path)
  (call-with-input-file	 	
      path	 	 	 	 
    (lambda (port) 
      (define g (make-vector (read-problem port path) '()))
      (let loop ([line (read-line port 'any)])
        (match line
          [(? eof-object?) 
           (vector-map! reverse g)
           g]
          [(regexp #px"^\\s*e\\s+(\\d+)\\s+(\\d+)\\s*$" 
                   (list _ (app id->idx src) (app id->idx target)))
           (vector-set! g src (cons target
                                    (vector-ref g src)))
           (loop (read-line port 'any))]
          [_ (loop (read-line port 'any))])))
    #:mode 'text))

(define (id->idx str) (sub1 (string->number str)))

; Reads the port searching for the problem specification, 
; and returns the number of nodes in the graph if the spec is 
; found.  Otherwise throws an error.
(define (read-problem port path)
  (let loop ([line (read-line port 'any)])
    (match line
      [(? eof-object?) 
       (raise-argument-error 'read-problem "DIMACS graph file" path)]
      [(or (regexp #px"^\\s*p\\s+edges?\\s+(\\d+)\\s+\\d+\\s*$" (list _ nodes))
           (regexp #px"^\\s*p\\s+col\\s+(\\d+)\\s+\\d+\\s*$" (list _ nodes)))
       (string->number nodes)]
      [_ (loop (read-line port 'any))])))
       

; We represent a coloring as a vector of natural numbers. 
; The coloring for a graph g has the same length as (node-count g).
(define coloring/c (vectorof natural-number/c #:flat? #t))

; Returns the color for the given node (index) in the specified coloring.
(define color-ref vector-ref)

; Returns the number of nodes in the specified coloring
(define color-size vector-length)

; This parameter holds the absolute path to the dot executable.
(define dot 
  (make-parameter (find-executable-path "dot")))

; Given a graph and, optionally, a coloring specification,
; produces a dot visualization of the graph and coloring.
; If the coloring includes more than 21 colors, no colors 
; are displayed.
(define (visualize g [coloring #f])
  (and coloring (not (= (node-count g) (vector-length coloring)))
       (raise-argument-error 'visualize "a valid coloring?" coloring))
  (define-values (p p-out p-in p-err)
    (subprocess #f #f #f (dot) "-Tpng"))
  (graph->dot g coloring p-in)
  (close-output-port p-in)
  (read-bitmap p-out 'png))
  
; Given a graph and, optionally, a coloring specification,
; produces a dot file for the graph and coloring.
; If the coloring includes more than 21 colors, no colors 
; are displayed.
(define (graph->dot g [raw-coloring #f] [port (current-output-port)])
  (fprintf port "digraph G {\n")
  (fprintf port "margin=0;\n")
  (fprintf port "size=\"64,64\";\n")
  (define coloring (colors->palette raw-coloring))
  (when coloring
    (for ([n (nodes g)])
      (fprintf port "~a [shape=circle,style=filled,fillcolor=~s];\n" n (color-ref coloring n))))
  (for* ([n (nodes g)] [child (children g n)])
    (fprintf port "~a -> ~a;\n" n child))
  (fprintf port "}\n"))

(define (rgb->dot rgb)
  (apply string-append "#" 
         (for/list ([component rgb])
           (~r component #:base 16))))

(define (colors->palette coloring)
  (and coloring
       (let ([colors (remove-duplicates (vector->list coloring))])
         (and (<= (length colors) (length palette))
              (let ([colormap (for/hash ([color (sort colors <)][rgb palette])
                                (values color rgb))])
                (for/vector ([c coloring])
                  (hash-ref colormap c)))))))
                
              
(define palette
  '("#51574a" "#447c69" "#74c493"
    "#8e8c6d" "#e4bf80" "#e9d78e"
    "#e2975d" "#f19670" "#e16552"
    "#c94a53" "#be5168" "#a34974"
    "#993767" "#65387d" "#4e2472"
    "#9163b6" "#e279a3" "#e0598b"
    "#7c9fb0" "#5698c4" "#9abf88")) 
  