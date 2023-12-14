#lang racket

;; Exercises 2: rectangle library

(provide rect
         rect?
         rect-area
         rect-intersect
         rect-list-intersect
         all-sub-rectangles)

; any two opposing corners of a grid-aligned rectangle as pairs (x0,y0), (x1,y1)
; --> `(rect ,lower-left-x ,lower-left-y ,upper-right-x ,upper-right-y) 
(define (rect x0 y0 x1 y1)
  ; return a normalized rect-tagged s-expr representation of the rectangle
  (list 'rect (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1)))

; Predicate defining a rectangle
(define (rect? r)
  (match r
         [`(rect ,x0 ,y0 ,x1 ,y1)
          (and (andmap integer? `(,x0 ,x1 ,y0 ,y1))
               (<= x0 x1)
               (<= y0 y1))]
         [else #f]))

; Given a rect?, yield its (integer?) area
(define (rect-area rect)
  (* (- (list-ref rect 3) (list-ref rect 1)) (- (list-ref rect 4) (list-ref rect 2))))

; Compute the rectangular intersection of any two rectangles
; If there is no intersection, return a rectangle with 0 area.
(define (x5 rect0 rect1) (max (list-ref rect0 1) (list-ref rect1 1)))
(define (y5 rect0 rect1) (max (list-ref rect0 2) (list-ref rect1 2)))
(define (x6 rect0 rect1) (min (list-ref rect0 3) (list-ref rect1 3)))
(define (y6 rect0 rect1) (min (list-ref rect0 4) (list-ref rect1 4)))

(define (rect-intersect rect0 rect1)
  (if [or (> (x5 rect0 rect1)
             (x6 rect0 rect1))
          (> (y5 rect0 rect1)
             (y6 rect0 rect1))]
      [0]
      [list 'rect (x5 rect0 rect1) (y5 rect0 rect1) (x6 rect0 rect1) (y6 rect0 rect1)]))

; Compute the intersection of a list of one or more rectangles
; E.g., the list `((rect 0 0 10 10) (rect 0 -5 10 1) (rect -5 -5 2 5))
;       has intersection `(rect 0 0 2 1)
(define (rect-list-intersect rect-list)
  (define inter1 (rect-intersect (list-ref rect-list 0) (list-ref rect-list 1)))
  (define inter2 (rect-intersect (list-ref rect-list 0) (list-ref rect-list 2)))
  (define inter3 (rect-intersect (list-ref rect-list 1) (list-ref rect-list 2)))
  (rect-intersect (rect-intersect inter1 inter2) inter3)
)

; Compute a Racket (set) of all sub-rectangles in the given rectangle
; We will call any rectangle r', with integer side-lengths of at least 1, a "sub-rectangle" of r iff r fully contains r'
; E.g., (all-sub-rectangles (rect 0 0 0 0)) => (set)
; E.g., (all-sub-rectangles (rect 0 0 1 1)) => (set `(rect 0 0 1 1))
; E.g., (all--sub-rectangles (rect 10 5 11 7)) => (set `(rect 10 5 11 7) `(rect 10 5 11 6) `(rect 10 6 11 7))
; Hint: can you solve this using the `foldl` and `range` functions?
(define (all-sub-rectangles r)
  (define (split-list lst n)
    (if (not (empty? lst))
        (cons (take lst n) (split-list (drop lst n) n))
        '()))
  (define (get-sub-rectangles)
    (let ([x0 (list-ref r 1)]
          [y0 (list-ref r 2)]
          [x1 (list-ref r 3)]
          [y1 (list-ref r 4)])
      (for/list ([new-x0 (range x0 (+ x1 1))])
        (for/list ([new-y0 (range y0 (+ y1 1))])
         (for/list ([new-x1 (range (+ new-x0 1) (+ x1 1))])
          (for/list ([new-y1 (range (+ new-y0 1) (+ y1 1))])
            `(set ,new-x0 ,new-y0 ,new-x1 ,new-y1)))))))
  (split-list (flatten (get-sub-rectangles)) 5))