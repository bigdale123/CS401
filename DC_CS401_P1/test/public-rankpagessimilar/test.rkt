#lang racket

(require "../../pagerank.rkt")

(with-output-to-file "output"
  (lambda ()
    (print (rank-pages '#hash(
       (node0 . 1/10)
       (node1 . 1/10)
       (node2 . 1/10)
       (node3 . 1/10)
       (node4 . 23942/100000)
       (node5 . 1/10))))))
