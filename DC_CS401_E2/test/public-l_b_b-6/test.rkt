#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (set-count (all-sub-rectangles (rect 5 5 5 5))))))
