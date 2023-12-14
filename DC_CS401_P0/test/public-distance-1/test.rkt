#lang racket

(require "../../p0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-distance 1 1 49 56))))
