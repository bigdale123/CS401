#lang racket

;; Exercises 5: free variables and capture avoiding substitution

(provide exp?
         free-vars
         capt-avoid-subst)

; A predicate for terms in the lambda calculus
(define (exp? e)
  (match e
         [(? symbol?) #t]
         [`(,(? exp?) ,(? exp?)) #t]
         [`(lambda (,(? symbol?)) ,(? exp?)) #t]
         [_ #f]))

; Takes an arbitrary exp? and returns a set of its free variables
; E.g., (free-vars '(lambda (x) (f (g x)))) => (set 'f 'g)
(define (free-vars exp)
  (match exp
          [`(lambda (,arg) ,body) (remv arg (free-vars body))]
          [`(,operator ,operand) (set-union (free-vars operator) (free-vars operand))]
          [_ (list exp)]
          ))

; Capture avoiding substitution:
; Takes an expression e0, and returns it, except with every instance of x within replaced with e1: 
;   i.e, e0[x <- e1]
; However, you must care to avoid capturing a variable incorrectly, recall that, via beta reduction:
;   ((lambda (y) (lambda (y) y)) (lambda (x) x))  -->beta  (lambda (y) y)   --- inner y shadows the outer
; If a substitution is not permitted (i.e., because it would first require an administrative 
; renaming---an alpha-reduction), return 'failed instead
;   E.g., (capt-avoid-subst ) => 'failed
(define (capt-avoid-subst e0 x e1)
  
  (match e0
      [(? symbol? ele) #:when (equal? x ele) e1]
      [(? symbol? ele) ele]
      [`(lambda (,y) ,body) #:when (equal? x y) e0]
      [`(lambda (,y) ,body) #:when (and (not (set-member? (free-vars e1) y)) (not (equal? x y))) (define body+ `(lambda (,y) ,(capt-avoid-subst body x e1))) (if (equal? body+ 'failed) 'failed body+)]
      [`(,ef ,ea) (define ef+ (capt-avoid-subst ef x e1)) (define ea+ (capt-avoid-subst ea x e1)) (if (or (equal? ef+ 'failed) (equal? ea+ 'failed)) 'failed `(,ef+ ,ea+))]
      [_ 'failed]
   ))


