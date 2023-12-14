#lang racket

;; Assignment 4: A church-compiler for Scheme, to Lambda-calculus

(provide church-compile
         ; provided conversions:
         church->nat
         church->bool
         church->listof)


;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)    
;     | (let ([x e] ...) e)  
;     | (let* ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (e e ...)    
;     | x  
;     | (and e ...) | (or e ...)
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f 
; nat ::= 0 | 1 | 2 | ... 
; x is a symbol
; prim is a primitive operation in list prims
; The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

; This input language has semantics identical to Scheme / Racket, except:
;   + You will not be provided code that yields any kind of error in Racket
;   + You do not need to treat non-boolean values as #t at if, and, or forms
;   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
;                                           or binary (+ - * = cons)
;   + There will be no variadic functions or applications---but any fixed arity is allowed

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;
; also as interpreted by Racket


;; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '())))


;; Write your church-compiling code below:

; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)
(define (churchify e)
  (match e
        ; Let
        ; This isn't what was in the p3 intro video (and it's super wacky)
        ;   BUT it's the only way i've found that handles the k-ary let scheme 
        [`(let ([,x ,rhs] ...) ,body) (churchify `((lambda ,x ,body) .,rhs))]
        [`(let () ,body) (churchify body)] 

        ; Let*
        [`(let* (,x0 ,xs ...) ,body) (churchify `(let (,x0) (let* (,@xs) ,body)))]
        [`(let* () ,body) (churchify body)]

        ;letrec
        [`(letrec ([,f ,y] ...) ,body) (churchify `())]

        ; Curry Lambdas
        [`(lambda () ,body) `(lambda (_) ,(churchify body))]
        [`(lambda (,x) ,body) `(lambda (,x) ,(churchify body))]
        [`(lambda (,x ,ys ...) ,body) `(lambda (,x) ,(churchify `(lambda ,ys ,body)))]

        ; Logic stuff
        [`(if ,ge ,te ,fe) (churchify `(,ge (lambda (_) ,te) (lambda (_) ,fe)))]

        

        
        

        ; Literals
        [(? integer? n)
          (define (wrap n)
            (if (zero? n)
              'x
              `(f ,(wrap (- n 1)))))
          (churchify `(lambda (f x) ,(wrap n)))
        ]
        [''() (churchify '(lambda (cons null) (null)))]
        ; Why does this (x x) work and x alone does not???
        [#t `(lambda (x) (lambda (y) (x x)))]
        [#f `(lambda (x) (lambda (y) (y y)))]

        ; Curry Applications
        ; moved to bottom because we want to match every possible thing before currying
        [`(,ef) `(,(churchify ef) (lambda (x) x))]
        [`(,ef ,ea) `(,(churchify ef) ,(churchify ea))]
        [`(,ef ,earg0 ,eargs ...) (churchify `((,ef ,earg0) ,@eargs))]

        [_ e]
  ))

; Takes a whole program in the input language, and converts it into an equivalent program in lambda-calc
(define (church-compile program)
  ; Define primitive operations and needed helpers using a top-level let form?
  (define todo `(lambda (x) x))
  (define church+ `(lambda (m n) (lambda (f x) (n f (m f x)))))
  (define church* `(lambda (m n) (lambda (f x) ((m (n f)) x))))
  (define church-add1 `(lambda (n) (lambda (f x) (f ((n f) x)))))
  (define church-null? `(lambda (n) (n (lambda (x y) #f) (lambda () #t))))
  (define church-not `(lambda (n) (if n #f #t)))
  (define church-cons `(lambda (n m) (lambda (cons null) (cons n m))))
  (define church-car `(lambda (n) (n (lambda (x y) x) (lambda () (lambda (n) n)))))
  (define church-cdr `(lambda (n) (n (lambda (x y) y) (lambda () (lambda (m) m)))))
  (define church-sub1 `(lambda (n) (lambda (x) (lambda (y) (((n (lambda (g) (lambda (h) (h (g f)))))(lambda (_) y)) (lambda (x) x))))))
  (define church-minus `(lambda (m n) ((n ,church-sub1) m)))
  (churchify
   `(let 
      (
        [+ ,church+]
        [* ,church*]
        [- ,church-minus]
        [add1 ,church-add1]
        [null? ,church-null?]
        [not ,church-not]
        [cons ,church-cons]
        [car ,church-car]
        [cdr ,church-cdr]
      )
      ,program)))

