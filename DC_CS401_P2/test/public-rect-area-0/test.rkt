#lang racket

(require "../../rectangle-coverage.rkt")


(with-output-to-file "output"
                     (lambda () 
                       (print (total-rect-area '((rect 1 1 4 4)
                                                 (rect 3 3 6 6)
                                                 (rect 2 2 3 7)
                                                 (rect 1 1 1 1)
                                                 (rect 2 2 2 2)
                                                 (rect 9 9 9 9)
                                                 (rect 1 1 2 2)
                                                 (rect 1 1 2 3)
                                                 (rect 1 1 3 2)
                                                 (rect 2 2 2 3)
                                                 (rect 2 2 3 3)
                                                 (rect 2 2 3 3)
                                                 (rect 4 4 5 5)
                                                 (rect 4 4 5 6)
                                                 (rect 4 4 6 5)
                                                 (rect 5 5 6 6)
                                                 (rect 4 5 5 6)
                                                 (rect 1 2 2 3)
                                                 (rect 1 1 2 3)
                                                 (rect 3 3 4 4)
                                                 (rect 4 4 6 6))))))
