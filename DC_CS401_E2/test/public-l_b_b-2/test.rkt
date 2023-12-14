#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (rect-list-intersect
                               `((rect 0 0 10 10) (rect 0 -5 10 1) (rect -5 -5 2 5))))))
