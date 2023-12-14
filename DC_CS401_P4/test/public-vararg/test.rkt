#lang racket

(require "../../interp-ce.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (interp-ce '((lambda a (cons 5 a)) 0 1 2 3 4))))
                     #:exists 'replace)
