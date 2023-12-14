#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (map is-balanced-tree?
                                   '(((0 . 1) . (a . b))
                                     (0 . (1 . 2))
                                     ((((1 . 2) . 3) . 4) . 5)
                                     (1 . 2))))))
