#lang racket

(require "../../e5.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (capt-avoid-subst '(lambda (a) (b b)) 'b 'c)))
                     #:exists 'replace)
