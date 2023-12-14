#lang racket

(require "../../e3.rkt")


(with-output-to-file "output"
                     (lambda () 
                       (print (total-rect-area '((rect 1 1 4 4) (rect 3 3 6 6) (rect 2 2 3 7))))))
