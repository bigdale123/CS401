#lang racket

;; Exercises 0: complete three simple functions and three simple expressions

(provide expr0
         expr1
         expr2
         nor-value
         least-of-three
         hypotenuse-length)

; Change 'todo to 4 so that expr0 is defined as 3*4+5 in s-expression notation
(define expr0 (+ (* 3 4) 5))

; Define expr1 using an s-expression equivalent of 3*4*5-3*10
; (do not evaluate, translate the above expression into a s-expression)
(define expr1 (- (* (* 3 4) 5) (* 3 10)))

; Define expr2 using an s-expression equivalent of 10-4-2*1-1
(define expr2 (- (- (- 10 4) (* 2 1)) 1))

; Compute the truth value of the proposition "~(x \/ y)" (i.e., "not of (x or y)", or "x nor y") where x and y are booleans
(define (nor-value x y)
  (nor x y))

; Compute the least of three integers
(define (least-of-three x y z)
  (min x y z))

; Compute the length of the Hypotenuse of a triangle with perpendicular edges of length a and b 
(define (hypotenuse-length a b)
  (sqrt (+ (* a a) (* b b))))


