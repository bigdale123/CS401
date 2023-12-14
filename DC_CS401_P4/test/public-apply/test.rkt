#lang racket

(require "../../interp-ce.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (interp-ce '(+ 1 (apply * (list 1 2 3 4 5))))))
                     #:exists 'replace)
