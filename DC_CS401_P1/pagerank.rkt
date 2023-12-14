#lang racket

;; Project 1: Implementing PageRank
;;
;; PageRank is a popular graph algorithm used for information
;; retrieval and was first popularized as an algorithm powering
;; the Google search engine. Details of the PageRank algorithm will be
;; discussed in class. Here, you will implement several functions that
;; implement the PageRank algorithm in Racket.
;;
;; Hints: 
;; 
;; - For this project, you may assume that no graph will include
;; any "self-links" (pages that link to themselves) and that each page
;; will link to at least one other page.
;;
;; - you can use the code in `testing-facilities.rkt` to help generate
;; test input graphs for the project. The test suite was generated
;; using those functions.
;;
;; - You may want to define "helper functions" to break up complicated
;; function definitions.

(provide graph?
         pagerank?
         num-pages
         num-links
         get-backlinks
         mk-initial-pagerank
         step-pagerank
         iterate-pagerank-until
         rank-pages)

;; This program accepts graphs as input. Graphs are represented as a
;; list of links, where each link is a list `(,src ,dst) that signals
;; page src links to page dst.
;; (-> any? boolean?)
(define (graph? glst)
  (and (list? glst)
       (andmap
        (lambda (element)
          (match element
                 [`(,(? symbol? src) ,(? symbol? dst)) #t]
                 [else #f]))
        glst)))

;; Our implementation takes input graphs and turns them into
;; PageRanks. A PageRank is a Racket hash-map that maps pages (each 
;; represented as a Racket symbol) to their corresponding weights,
;; where those weights must sum to 1 (over the whole map).
;; A PageRank encodes a discrete probability distribution over pages.
;;
;; The test graphs for this assignment adhere to several constraints:
;; + There are no "terminal" nodes. All nodes link to at least one
;; other node.
;; + There are no "self-edges," i.e., there will never be an edge `(n0
;; n0).
;; + To maintain consistenty with the last two facts, each graph will
;; have at least two nodes.
;; + There will be no "repeat" edges. I.e., if `(n0 n1) appears once
;; in the graph, it will not appear a second time.
;;
;; (-> any? boolean?)
(define (pagerank? pr)
  (and (hash? pr)
       (andmap symbol? (hash-keys pr))
       (andmap rational? (hash-values pr))
       ;; All the values in the PageRank must sum to 1. I.e., the
       ;; PageRank forms a probability distribution.
       (= 1 (foldl + 0 (hash-values pr)))))

;; Takes some input graph and computes the number of pages in the
;; graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0,
;; n1, and n2.
;;
;; (-> graph? nonnegative-integer?)
(define (num-pages graph)
  (length (set->list (list->set (flatten graph)))))

;; Takes some input graph and computes the number of links emanating
;; from page. For example, (num-links '((n0 n1) (n1 n0) (n0 n2)) 'n0)
;; should return 2, as 'n0 links to 'n1 and 'n2.
;;
;; (-> graph? symbol? nonnegative-integer?)
(define (num-links graph page)
  ; links=0
  ; for i in graph:
  ;   if car(i) == page:
  ;     links += 1
  (define (num-links-inner graph page acc)
    (if (null? graph)
        acc
        (if (equal? (caar graph) page)
            (num-links-inner (cdr graph) page (+ 1 acc))
            (num-links-inner (cdr graph) page acc)))
  )
  (num-links-inner graph page 0)
)


;; Calculates a set of pages that link to page within graph. For
;; example, (get-backlinks '((n0 n1) (n1 n2) (n0 n2)) n2) should
;; return (set 'n0 'n1).
;;
;; (-> graph? symbol? (set/c symbol?))
(define (get-backlinks graph page)
  ; total_baclinks = []
  ; for i in graph:
  ;   if cdr(i) == page:
  ;     total_backlinks.append(car(i))
  ; dedeuplicate(i)
  (define (backlinks-inner pair acc)
    (if (equal? (cadr pair) page)
        (cons (car pair) acc)
        acc
    )
  )
  (list->set (foldr backlinks-inner '() graph))
)


;; Generate an initial pagerank for the input graph g. The returned
;; PageRank must satisfy pagerank?, and each value of the hash must be
;; equal to (/ 1 N), where N is the number of pages in the given
;; graph.
;; (-> graph? pagerank?)
(define (mk-initial-pagerank graph)
  ; Get unique pages
  (define unique (set->list (list->set (flatten graph))))
  (define (mk-inner h g)
    (if (null? g)
        h
        (mk-inner (hash-set h (car g) (/ 1 (length unique))) (cdr g))
    )
  )
  (mk-inner (hash) unique)
)

;; Perform one step of PageRank on the specified graph. Return a new
;; PageRank with updated values after running the PageRank
;; calculation. The next iteration's PageRank is calculated as
;;
;; NextPageRank(page-i) = (1 - d) / N + d * S
;;  
;; Where:
;;  + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
;;  + N is the number of pages in the graph
;;  + S is the sum of P(page-j) for all page-j.
;;  + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
;;  + NumLinks(page-j) is the number of outbound links of page-j
;;  (i.e., the number of pages to which page-j has links).
;;
;; (-> pagerank? rational? graph? pagerank?)
(define (step-pagerank pr d graph)
  (define keys (hash-keys pr))
  (define N (num-pages graph))
  (define (pofj page)
    (/ (hash-ref pr page) (num-links graph page))
  )
  (define (add-pofj key acc)
    (+ acc (pofj key))
  )
  (define (s exclude-page) 
    (foldl add-pofj 0 (set->list (get-backlinks graph exclude-page)))
  )
  (define (updater page)
    (+ (/ (- 1 d) N) (* d (s page)))
  )
  (define (step-pagerank-inner keys h)
    (if (null? keys)
        h
        (step-pagerank-inner (cdr keys) (hash-set h (car keys) (updater (car keys))))
    )
  )
  ;(step-pagerank-inner keys pr)
  (step-pagerank-inner keys pr)
)

;; Iterate PageRank until the largest change in any page's rank is
;; smaller than a specified delta.
;;
;; (-> pagerank? rational? graph? rational? pagerank?)
(define (iterate-pagerank-until pr d graph delta)
  (define keys (hash-keys pr))
  (define pr+ (step-pagerank pr d graph))
  (define (calculate-delta pr pr+)
    (foldl (lambda (key acc) (cons (- (hash-ref pr+ key) (hash-ref pr key)) acc)) '() keys)
  )
  ; Used the following stack overflow thread to find the largest delta (max doesnt work with fractions?)
  ; https://stackoverflow.com/questions/27128960/getting-the-largest-number-in-a-list-in-scheme
  (define (getlargest lst)
    (if (null? lst)
        #f
        (foldl (lambda (e r) (if (> e r) e r))
              (car lst)
              (cdr lst))
    )
  )
  (if (<= (getlargest (calculate-delta pr pr+)) delta)
        pr+
        (iterate-pagerank-until pr+ d graph delta))
  
)

;; Given a PageRank, returns the list of pages it contains in ranked
;; order (from least-popular to most-popular) as a list. You may
;; assume that the none of the pages in the pagerank have the same
;; value (i.e., there will be no ambiguity in ranking)
;;
;; (-> pagerank? (listof symbol?))
(define (rank-pages pr)
  (define (strip-cdr l) (foldr (lambda (element acc) (cons (car element) acc)) '() l))
  (define (compare-cdr element1 element2) (< (cdr element1) (cdr element2)))
  ; (strip-cdr (hash->list pr))
  (strip-cdr (sort (hash->list pr) compare-cdr))
)
