#lang racket

;; Assignment 2: Quad-trees and immutable data-structures

(provide total-rect-area)

; Got idea for checking overlap here: https://www.geeksforgeeks.org/find-two-rectangles-overlap/
(define (detect-overlap rect1 rect2)
  (define y-ulimit-1 (cadddr rect1))
  (define x-ulimit-1 (caddr rect1))
  (define y-blimit-1 (cadr rect1))
  (define x-blimit-1 (car rect1))
  (define y-ulimit-2 (cadddr rect2))
  (define x-ulimit-2 (caddr rect2))
  (define y-blimit-2 (cadr rect2))
  (define x-blimit-2 (car rect2))
  (cond
    ; check if any rectangle is essentially a line
    ((or (equal? x-blimit-1 x-ulimit-1) (equal? y-blimit-1 y-ulimit-1) (equal? x-blimit-2 x-ulimit-2) (equal? y-blimit-2 y-ulimit-2)) #f)
    ; check if one rectangle is on left side of other
    ((or (>= x-blimit-1 x-ulimit-2) (>= x-blimit-2 x-ulimit-1)) #f)
    ; check if one rectangle is above another
    ((or (>= y-blimit-2 y-ulimit-1) (>= y-blimit-1 y-ulimit-2)) #f)
    ; else, must overlap
    (else #t)
  )
)

(define (is-rect rect)
  ; if given rectangle is not a rectangle, area=0 (meaning, represents point or line, which has area 0)
  (define y-ulimit-1 (cadddr rect))
  (define x-ulimit-1 (caddr rect))
  (define y-blimit-1 (cadr rect))
  (define x-blimit-1 (car rect))
  (if (or (equal? x-blimit-1 x-ulimit-1) (equal? y-blimit-1 y-ulimit-1)) #f #t)
)

(define (get-unit-squares rect acc)
  ; get list of unit squares for one rectangle
  ; Update: Too Slow :( but keeping. Works well on small systems.
  ; Unless, i can cut out this process for all rectangles known to not overlap.
  ; Which for those that dont overlap at all, calculating area is easy.
  (define y-ulimit (cadddr rect))
  (define x-ulimit (caddr rect))
  (define y-blimit (cadr rect))
  (define x-blimit (car rect))
  (define x-vals (range x-blimit x-ulimit))
  (define y-vals (range y-blimit y-ulimit))
  (foldl (lambda (y-val acc) (foldl (lambda (x-val acc) (cons (flatten (cons x-val y-val)) acc)) acc x-vals)) acc y-vals)
)

(define (strip-rect rect-lst)
  (foldr (lambda (pair acc) (cons (cdr pair) acc)) '() rect-lst))

(define (total-area-helper rect-list)
  (remove void (foldr (lambda (rect acc) (cond ((is-rect rect)(get-unit-squares rect acc))(else acc))) '() (strip-rect rect-list)))
)

; total-rect-area: returns the total integer area covered by any (one or more) rectangles in the given list 
; Don't double-count. Should be identical to the same solution from e3.rkt, except this version must be in O(n log n)
; Hint: implement an immutable quad-tree to represent 2D space; FYI, a solution takes only ~30-40 lines of code.
(define (total-rect-area rect-list)
  (length (set->list (list->set (total-area-helper rect-list)))))


(total-rect-area '((rect 1 1 1 4)
                  (rect 0 0 0 6)
                  (rect 6 6 2 6)
                  (rect 1 1 1 1)
                  (rect 2 2 2 2)
                  (rect 999 9 999 9)
                  (rect 0 0 9999999999999 9999999999)))


