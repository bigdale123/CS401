#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (map filter-numbers '((1 2 a 3) (a b c) (q 1 a 2 b c d e))))))
