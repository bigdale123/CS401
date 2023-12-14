#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (take-front (drop-front '(1 2 3 4 5 6 7 8 9) 2) 3))))
