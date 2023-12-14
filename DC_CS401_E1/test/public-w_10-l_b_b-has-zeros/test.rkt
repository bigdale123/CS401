#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (map has-zeros? '((1 2 3) (1 0 2) (1 2 3 0 0))))))
