#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (map list-depth '((1 2 3) () ((1 2 ((3) 4 ((5))))))))))
