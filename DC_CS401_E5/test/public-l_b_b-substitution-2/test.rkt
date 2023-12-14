#lang racket

(require "../../e5.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (capt-avoid-subst '((lambda (x) x) (lambda (y) (x x))) 'x '(lambda (z) z))))
                     #:exists 'replace)
