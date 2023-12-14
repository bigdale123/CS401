#lang racket

(require "../../p0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (shortest-distance (cons 0 0) (cons 999 999) (cons 500 100)))))
