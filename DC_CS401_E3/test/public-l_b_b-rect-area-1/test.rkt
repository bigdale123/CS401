#lang racket

(require "../../e3.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (total-rect-area '((rect 2 2 7 7))))))
