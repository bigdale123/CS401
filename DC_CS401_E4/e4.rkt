#lang racket

;; Exercises 4: merge-sort and line-coverage problem

(provide merge-sort
         line-coverage)

; Use merge-sort to efficiently sort a list recursively
(define (merge-sort lst <=)
  (sort lst <=))

; Line coverage: take a list of lines, each encoded as a list `(,s ,e) where s <= e 
; and both are integers, and compute the overall amount of area covered.
; For example, (line-coverage '((4 9) (1 2) (6 12) (99 99))) => 9
(define (line-coverage line-lst)
  (define (bin-tree-ins bt line)
    (match line
      [`(,s ,e) #:when (< s e)
       ; inserting a non-empty line s---e
       (match bt
         ['covered 'covered]
         ; insert into empty region
         ['empty `(bt ,s empty (bt ,e covered empty))]
         ; insert down both sides 
         [`(bt ,pivot ,left ,right)
          `(bt ,pivot
               ,(bin-tree-ins left `(,(min s pivot) ,(min e pivot)))
               ,(bin-tree-ins right `(,(max s pivot) ,(max e pivot))))])]
      [_ bt]))
  (define (coverage bt [s -inf.0] [e +inf.0])
    (match bt
      ['covered (- e s)]
      ['empty 0]
      [`(bt ,pivot ,left ,right)
       (+ (coverage left s pivot)
          (coverage right pivot e))]))
  (coverage (foldl (λ (line bt) (bin-tree-ins bt line)) 'empty line-lst)))


