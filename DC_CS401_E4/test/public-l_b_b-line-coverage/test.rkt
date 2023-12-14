#lang racket

(require "../../e4.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (line-coverage '((4 9) (1 2) (6 12) (99 99)))))
                     #:exists 'replace)
