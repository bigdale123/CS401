#lang racket

(require "../../e3.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (define ps (power-set (set)))
                       (print (set-count ps))))
