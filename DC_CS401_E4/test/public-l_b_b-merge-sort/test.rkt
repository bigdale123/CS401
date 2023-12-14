#lang racket

(require "../../e4.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (merge-sort '(13 3 4 7 8 17 5 10 15 9 11 6 12 2 14 16) <=)))
                     #:exists 'replace)
