#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (rect-intersect (rect 0 8 8 1) (rect 3 5 5 12)))))
