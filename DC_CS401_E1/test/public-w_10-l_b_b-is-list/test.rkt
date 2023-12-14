#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (map is-list? '((1 2 3) () (1 2 . 3))))))
