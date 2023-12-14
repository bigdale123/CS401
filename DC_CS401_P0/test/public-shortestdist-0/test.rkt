#lang racket

(require "../../p0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (shortest-distance (cons 1 1) (cons 1 4) (cons 3 4)))))
