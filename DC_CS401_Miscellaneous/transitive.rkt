#lang racket

(define G '((1 2)(1 3)(3 4)(2 4)(4 5)(5 6)))

(define (add-edge g k v)
    ;this is funky, but this adds to the hash g to be a key (k) equal to a set containing the values of the
    ; old set, plus the new value that we found in the fold command
    (hash-set g k (set-add (hash-ref g k set) v)))

(define (assoc->hash a)
    (foldl (lambda (edge h)
                ;match-define will error if edge does not match the defined pattern of a list containing 2 elements
                (match-define (list k v) edge)
                ;add edge to hash
                (add-edge h (first edge) (second edge)))
           (hash)
           a))

(define (iter-to-fixpoint f x)
    (define x+ (f x))
    (if (equal? x x+)
        x
        (iter-to-fixpoint f x+)))

(define (add-paths g)
    ;;; pseudocode:
    ;;; for x in g:
    ;;;     for y in g(x):
    ;;;         for z in g(y):
    ;;;             (add-edge g x z)
    (foldl (lambda (x g+)
                (foldl (lambda (y g+)
                            (foldl (lambda (z g+)
                                        (add-edge g+ x z))
                                   g+
                                   (set->list (hash-ref g y set))))
                        g+
                        (set->list (hash-ref g x set))))
            g 
            (hash-keys g))

)   

(define (transitive-closure gh)
    (iter-to-fixpoint
        add-paths
        gh))


(define (filo-queue)
    '(filo ()))

(define (filo-enqueue q x)
    (match q ['(filo ,(? list? lst))
              '(filo ,(cons x lst))]))

(define (filo-dequeue q)
    (match q
        [`(filo (,x . ,tail))  (cons x '(filo ,tail))]
        [else (error "empty queue")]))




(add-paths (assoc->hash G))
(transitive-closure (assoc->hash G))
