#lang racket

(define (append lst0 lst1)
		(if (null? lst0)
			lst1
			(cons (car lst0) (append (cdr lst0) lst1))))
; This is direct style recursion

(define (foldl f acc list)
    (if (null? list)
        acc
        (foldl f (f (car list) acc) (cdr list))))

(define (foldr f acc list)
    (if (null? list)
        acc
        (f (car list) (foldr f acc (cdr list)))))

(define (reverse lst)
		(foldl cons '() lst))

(define (append lst0 lst1)
		(foldr cons lst1 lst0))

(define (my-map f lst)
    (if (null? lst)
        lst
        (cons (f (car lst)) (my-map f (cdr lst)))))

(define (avg-unique inp)
    (if (null? inp)
        0
        (/ (+ (length (set->list (list->set (car inp)))) (avg-unique (cdr inp))) (length inp))))

(define (iter-to-fixpoint f est)
    (define est+ (f est))
    (if (= est+ est)
        est
        (iter-to-fixpoint f est+)))

(define G '((1 2)(1 3)(3 4)(2 4)(4 5)(5 6)))

(reverse (list 1 2 3 4))
(append (list 1 2) (list 3 4))
(my-map add1 (list 1 2 3 4))

