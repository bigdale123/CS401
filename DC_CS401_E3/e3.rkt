#lang racket

;; Exercises 3: power-set and inclusion-exclusion principle

(provide power-set
         total-rect-area)

; power-set: takes an arbitrary Racket (set) and returns its power-set, the set of all its subsets
(define (power-set st)
  (let ([mylt (set->list st)])
    (if (null? mylt) '(())
      (let ((psetRest (power-set (cdr mylt)))) ;; let psetRest be all obtained subsets (ps (cdr set)) so we dont have to call it twice below
        (append psetRest ;; append the list of all current subsets to the new subsets obtained by adding (car set) to all current subsets
                (map (lambda (subset) ;; cons car to all subsets of pset rest to obtain the new subsets
                       (cons (car mylt) subset))
                     psetRest))))))



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
      [list 'rect 0 0 0 0]
      [list 'rect (x5 rect0 rect1) (y5 rect0 rect1) (x6 rect0 rect1) (y6 rect0 rect1)]))


; total-rect-area: takes a list of rectangles (defined in e2) and returns the total covered area
; Note: do not double-count area covered by >1 rectangles
; E.g., (total-rect-area '((rect 0 0 2 2) (rect 1 1 3 3))) => 7
; Hint: use the power-set function and the inclusion-exclusion principle; review your functions from e2


(define (inter-help rect)
  (if 
    (< (length rect) 3)
    (rect-intersect (first rect) (second rect))
    (inter-help (append (list (rect-intersect (first rect) (second rect))) (list-tail rect 2)))
  )
)

(define (area-helper rect acc)

  (cond  ((= (length rect)0) acc)
         ((= (length rect)1) (+ (rect-area (first rect)) acc))
         ((= (length rect)2) (- acc (rect-area (rect-intersect (first rect) (second rect)))))
         ((and (> (length rect)2) (= (modulo (length rect) 2) 0)) (- acc (rect-area (inter-help rect))))
         ((and (> (length rect)2) (> (modulo (length rect) 2) 0)) (+ (rect-area (inter-help rect)) acc))

  ))


(define (total-rect-area rect-list)
    (define work-set (power-set rect-list))
        (foldl area-helper 0 work-set)
    )

  


(total-rect-area '((rect 1 1 4 4)
                                                 (rect 3 3 6 6)
                                                 (rect 2 2 3 7)
                                                 (rect 1 1 1 1)
                                                 (rect 2 2 2 2)
                                                 (rect 9 9 9 9)
                                                 (rect 1 1 2 2)
                                                 (rect 1 1 2 3)
                                                 (rect 1 1 3 2)
                                                 (rect 2 2 2 3)
                                                 (rect 2 2 3 3)
                                                 (rect 2 2 3 3)
                                                 (rect 4 4 5 5)
                                                 (rect 4 4 5 6)
                                                 (rect 4 4 6 5)
                                                 (rect 5 5 6 6)
                                                 (rect 4 5 5 6)
                                                 (rect 1 2 2 3)
                                                 (rect 1 1 2 3)
                                                 (rect 3 3 4 4)
                                                 (rect 4 4 6 6)))