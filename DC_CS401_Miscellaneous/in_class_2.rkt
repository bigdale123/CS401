#lang racket

;Fifo (First in, First Out Queue)
(define (lazy-fifo-queue)
    '(fifo ()))

(define (lazy-fifo-enqueue q x)
    ; Runs in linear time, since we have to rebuild entire queue each time
    (match q ['(fifo ,(? list? lst))
              '(fifo ,(foldr cons (list x) lst))]))

(define (lazy-fifo-dequeue q)
    ; Runs in double liner time (?) since we have to tear down the list in O(N), and then rebuild in O(N)
    (match q
        [`(fifo (,x . ,tail))  (cons x '(fifo ,tail))]
        [else (error "empty queue")]))

; Okasaki - Functional Data Structures

;Idea is to use two lists to store queue. Only reverse when you need to by reversing the enqueue list
; into the dequeue list when dequeing.
; By using the two lists method, we only duplicate the reversed enqueue list when the dequeue list is empty.
; If the dequeue list is not empty, we dont need to be worried about updating it, since there's still stuff to dequeue
; before worrying about adding other elements to the dequeue list.

; Main idea is Amortized Complexity, every once in a while there is a heavy task, but for the most part
; The algorithm runs with frequent lighter tasks.

; first list is dequeue list, second is enqueue list
(define (two-list-fifo) '(fifo () ()))

(define (two-list-fifo-enqueue q x)
    (match q [`(fifo ,(? list? delst) ,(? list enlst))
              `(fifo ,delst ,(cons x enlst))]))

(define (two-list-fifo-dequeue q)
    (match q
        [`(fifo () ()) (error "empty queue")]
        [`(fifo (,x . ,tail) ,enlst) 
          (cons x `(fifo ,tail ,enlst))]
        [`(fifo () ,enlst) (two-list-fifo-dequeue `(fifo ,(reverse enlst) ()))]
        ))

(define q0 (two-list-fifo))
(pretty-print q0)
(define q11 (foldl (lambda (x q) (two-list-fifo-enqueue q x)) q0 '(1 2 3)))
(pretty-print q11)
(match-define (cons _ q2) (two-list-fifo-dequeue q11))
(pretty-print q2)
(define q3 (foldl (lambda (x q) (two-list-fifo-enqueue q x)) q2 '(5 6)))
(pretty-print q3)
(match-define (cons _ q4) (two-list-fifo-dequeue q3))
(pretty-print q4)
(match-define (cons _ q5) (two-list-fifo-dequeue q4))
(pretty-print q5)
(match-define (cons _ q6) (two-list-fifo-dequeue q5))
(pretty-print q4)