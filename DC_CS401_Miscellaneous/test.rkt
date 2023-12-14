#lang racket
(define (f x) (cond ( ( = ( modulo x 2 ) 0) (/ x 2) ) ( (= (modulo x 2) 1) (+ (* 3 x) 1) ) ) )

(f 2)
(f 3)