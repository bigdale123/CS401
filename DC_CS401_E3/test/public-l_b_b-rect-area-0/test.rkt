#lang racket

(require "../../e3.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (total-rect-area '()))))
