#lang racket

(require "../../e5.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (capt-avoid-subst '((lambda (a) a) (lambda (b) b)) 'x 'y)))
                     #:exists 'replace)
