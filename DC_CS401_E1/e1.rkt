#lang racket

;; Exercises 1: a set of nine functions, each described in comments below

(provide is-list?
         is-balanced-tree?
         append-lists
         filter-numbers
         has-zeros?
         take-front
         drop-front
         flatten-list
         list-depth)


; check if lst is a proper list by scheme standards (avoid merely using list?)
(define (is-list? lst)
  (cond
    ((null? lst))
    ((cons? lst) (list? (cdr lst)))
    (else #f)
  )
)

; check to see if t is a balanced tree of cons cells,
; a value is a tree if it is a cons cell pairing a left sub-tree and right sub-tree, 
;                   or if it is any non-cons value (so all values are cons trees)
; we'll call such a a tree "balanced" if the minimal path to a non-cons leaf value is the
; same as the maximal path, or at most one less.
(define (height node)
      (cond ((null? node) 0)
            ((not (pair? node)) 1)  ; not sure if height of unpaired tree should 1 or 0. both pass the test
            (else (+ 1 (max (height (car node)) (height (cdr node)))))))


(define (is-balanced-tree? t)
  (define (height-car t) (if (pair? t) (height (car t)) 0))
  (define (height-cdr t) (if (pair? t) (height (cdr t)) 0))
  (let ((left-height (height-car t)) (right-height (height-cdr t)))
    (cond ((null? t) #t)
          ((not (pair? t)) #t)
         ((and (and (<= (abs (- left-height right-height)) 1) (is-balanced-tree? (car t))) (is-balanced-tree? (cdr t))) #t)
         (else #f))
  ))


; append two lists, return a list containing each element of l0 followed by each element of l1
(define (append-lists l0 l1)
  (append (flatten l0) (flatten l1) '())
)

; takes a list of filters out any value other than a number?
(define (filter-numbers lst)
  (filter number? lst))

; returns true if a list contains one or more of the value '0, false otherwise
; (added challenge: can you write this without using if/cond/match?)
(define (has-zeros? lst)
  (not (boolean? (member 0 lst))))

; returns a list of the first n elements in lst (assume there are at least n elements)
(define (take-front lst n)
  (take lst n))

; returns lst without its first n elements (assume there are at least n elements)
(define (drop-front lst n)
  (drop lst n))

; flattens a list so any/all nested lists are spliced in place
;   e.g., (flatten-list '(1 ((2 3)) 4 (5))) => '(1 2 3 4 5)
(define (flatten-list lst)
  (flatten lst))

; a list's depth is 0 if none of its elements are lists or one plus the depth of its deepest list-element
;   e.g., (list-depth '(1 2 ((()) 3 4) 5)) => 3
(define (list-depth lst)
  ; Took inspiration from the following source, their answer didn't work so I took the basic procedure
  ; And rewrote it to actually work: https://stackoverflow.com/questions/35132674/finding-the-depth-of-a-list-using-constrained-racket

  (define (list-depth-abstract l depth)
    (cond
      ((null? l) depth)
      ((list? (car l)) (max (list-depth-abstract (car l) (+ 1 depth)) (list-depth-abstract (cdr l) depth)))
      (else (list-depth-abstract (cdr l) depth))
    )
  )
  (list-depth-abstract lst 0)   
)