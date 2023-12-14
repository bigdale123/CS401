#lang racket

;; Exercises 6: Reducing (normalizing/simplifying) lambda calc terms by textual substitution
;;              using four different orders of evaluation (applicative, normal, CBV, CBN)
(provide collect-evaluation-trace
         applicative-order-reduce
         normal-order-reduce
         CBV-order-reduce
         CBN-order-reduce)

; A predicate for terms in the lambda calculus
(define (exp? e)
  (match e
         [(? symbol?) #t]
         [`(,(? exp?) ,(? exp?)) #t]
         [`(lambda (,(? symbol?)) ,(? exp?)) #t]
         [_ #f]))

; Free variables (may be useful for defining capt-avoid-subst)
(define (free-vars exp)
  (match exp
         [(? symbol? sym)
           (set sym)]
         [`(,(? exp? fn) ,(? exp? arg))
           (set-union (free-vars fn) (free-vars arg))]
         [`(lambda (,(? symbol? var)) ,(? exp? body))
           (set-subtract (free-vars body) (set var))]
         [_ exp]))

; Capture avoiding substitution
(define (capt-avoid-subst e0 x e1)
  (match e0
         [(? symbol? sym)
             (cond [(equal? sym x) e1]
                   [else sym])]
         [`(,(? exp? fn) ,(? exp? arg))
             (let ([new-fn (capt-avoid-subst fn x e1)]
                   [new-arg (capt-avoid-subst arg x e1)])
               (cond [(or (equal? new-fn 'failed) (equal? new-arg 'failed)) 'failed]
                     [else `(,new-fn ,new-arg)]))]
         [`(lambda (,(? symbol? var)) ,(? exp? body))
           (cond [(equal? var x) e0]
                 [(not (set-member? (free-vars e1) var))
                   `(lambda (,var) ,(capt-avoid-subst body x e1))]
                 [else 'failed])]
         [_ 'failed]))

; Reduce the given expression by exactly one beta-reduction using
; applicative evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no applicative order redex.
; Skip over any redexes that cannot be reduced using capture avoiding subst.
(define (applicative-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (applicative-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
          (define body-st (applicative-order-reduce body))
          (if (set-empty? body-st)
              ; No redex under this lambda:
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; This is the innermost+leftmost redex
                    (let ([body+ (capt-avoid-subst body y ea)])
                      (if (eq? body+ 'failed)
                          (set) ; reducing redex failed
                          (set body+)))
                    ; A redex under the argument expression was found
                    (set `((lambda (,y) ,body) ,(set-first ea-st)))))
              ; A redex under the lambda was reduced already
              (set `((lambda (,y) ,(set-first body-st)) ,ea)))]
         [`(,ef ,ea)
          (define ef-st (applicative-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))

; Reduce the given expression by exactly one beta-reduction using
; normal evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no normal-order redex.
; Skip over any redexes that cannot be reduced using capture avoiding subst.
(define (normal-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (normal-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
          (define body+ (capt-avoid-subst body y ea))
          (if (eq? body+ 'failed)
              ; reducing redex failed
              (set)
              ; able to perfrom substitution
              (set body+))]
         [`(,ef ,ea)
          (define ef-st (normal-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (normal-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))

; Reduce the given expression by exactly one beta-reduction using
; call-by-value evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no CBV order redex.
(define (CBV-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          ; do not eval under a lambda
          (set)]
         [`((lambda (,y) ,body) ,ea)
          ; ignore eval under a lambda, jump straight to ea
          (let ([ea-st (applicative-order-reduce ea)])
            (if (set-empty? ea-st)
                ; This is the innermost+leftmost redex
                (let ([body+ (capt-avoid-subst body y ea)])
                  (if (eq? body+ 'failed)
                      (set) ; reducing redex failed
                      (set body+)))
                ; A redex under the argument expression was found
                (set `((lambda (,y) ,body) ,(set-first ea-st)))))]
         [`(,ef ,ea)
          (define ef-st (applicative-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))

; Reduce the given expression by exactly one beta-reduction using
; call-by-name evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no applicative order redex.
(define (CBN-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          ; do not eval under a lambda
          (set)]
         [`((lambda (,y) ,body) ,ea)
          (define body+ (capt-avoid-subst body y ea))
          (if (eq? body+ 'failed)
              ; reducing redex failed
              (set)
              ; able to perfrom substitution
              (set body+))]
         [`(,ef ,ea)
          (define ef-st (normal-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (normal-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))

; Takes one of the four step/reduce functions and an expression in the lambda calculus (satisfying exp?)
; Yields a list representing the full evaluation trace from e to a value
; Note, this function will non-terminate on programs like Omega that cannot be reduced to a value.
(define (collect-evaluation-trace step-f e)
  (let loop ([latest (set e)]
             [trace '()])
    (if (set-empty? latest)
        (reverse trace)
        (loop (step-f (set-first latest))
              (cons (set-first latest) trace)))))

