#lang racket

;; Document Model and associated helper functions

(provide (all-defined-out))

(require srfi/13)

;; The Document is composed of a text buffer represented as a string and a cursor position
;; represented as a pair (row . col)
(struct Document (text cursor) #:transparent)

;; Translates a (row . col) location to a index into the text
(define (loc->offset text loc)
  (apply + (cdr loc)
         (map (compose add1 string-length)
              (take (string-split text "\n") (car loc)))))

;; Translates an index into the text into a (row . col) locations
(define (offset->loc text offset)
  (if (zero? offset) '(0 . 0)
      (let ([lines (string-split
                    (substring text 0 (min offset (string-length text))) "\n" #:trim? #f)])
        (cons (sub1 (length lines)) (string-length (last lines))))))

;; Given 2 strings returns the locations of the deleted segment if the only differance was a deleted
;; segment, otherwise false
(define (deleted-locs before after)
  (define indices (infix-indices before after))
  (if indices (map (λ (i) (offset->loc before i)) indices) #f))

;; given 2 strings composed of substings x,y,z all of which may be empty, returns the indicies in the
;; xyz string representing the start and end of y
(define (infix-indices xyz xz)
  (cond
    [(string-prefix? xz xyz)
     (list (string-length xz) (string-length xyz))]
    [(string-suffix? xz xyz)
     (list 0 (- (string-length xyz) (string-length xz)))]
    [(string-suffix? (substring xz (string-prefix-length xyz xz)) xyz)
     (list (string-prefix-length xyz xz)
           (- (string-length xyz) (string-length (substring xz (string-prefix-length xyz xz)))))]
    [else #f]))

;; Given a document and a start and end index returns a new document where the text between the
;; indicies has been deleted
(define (delete-text doc start end)
  (define text (Document-text doc))
  (Document (string-append (substring text 0 (min (string-length text) (loc->offset text start)))
                           (substring text (min (string-length text) (loc->offset text end))))
            (Document-cursor doc)))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-equal? (loc->offset "hello\nworld" '(1 . 2)) 8)
  (check-equal? (offset->loc "hello\nworld" 8) '(1 . 2))
  (check-equal? (offset->loc "hello\nworld" 0) '(0 . 0))

  (check-equal? (deleted-locs "abc" "bc") '((0 . 0) (0 . 1)))
  (check-equal? (deleted-locs "0123456789" "012345") '((0 . 6) (0 . 10)))
  (check-equal? (deleted-locs "0123456789" "01289") '((0 . 3) (0 . 8)))
  (check-equal? (deleted-locs "hello\nworld" "hllo\nworld") '((0 . 1) (0 . 2)))
  (check-equal? (deleted-locs "hello\nworld" "ld") '((0 . 0) (1 . 3)))
  (check-equal? (deleted-locs "hello\nworld" "helly\nwerld") #f)
  (check-equal? (deleted-locs "hello\nworld" "h") '((0 . 1) (1 . 5)))
  (check-equal? (deleted-locs "this is a string" "this string") '((0 . 5) (0 . 10)))

  (check-equal? (delete-text (Document "abc\ndef" '(0 . 1)) '(0 . 2) '(1 . 1))
                (Document "abef" '(0 . 1))))

