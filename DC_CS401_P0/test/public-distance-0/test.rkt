#lang racket

(require "../../p0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-distance 4 5 8 8))))
