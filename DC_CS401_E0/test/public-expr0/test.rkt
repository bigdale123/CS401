#lang racket

(require "../../e0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print expr0)))