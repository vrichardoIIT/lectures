#lang racket

(require 2htdp/image) ; for a bit of fun with pictures

#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions
-----------------------------------------------------------------------------|#

((lambda (x y z)
   (* x (+ y z)))
 2 3 4)

((lambda (x y z)
  (println x)
  (println y)
  (println z)) 1 2 3)

(let ([f (lambda (x y z) 
           (* x (+ y z)))])
  (f 2 3 4))

(define (foo x y z)
  (* x (+ y z)))

(define (bar x y . z)
  (println x)
  (println y)
  (println z))


#|-----------------------------------------------------------------------------
;; Some more special forms

- `if`: if-then-else
- `when`: if-then
- `cond`: multi-way conditional
-----------------------------------------------------------------------------|#

#; (if bool-expr then-expr else-expr)

#; (cond [bool-expr1 result1]
         [bool-expr2 result2]
         [bool-expr3 result3]
         [else result4])


#|-----------------------------------------------------------------------------
;; Equality tests

- `=` for numbers
- `eq?` for pointer-based equality
- `equal?` for value-based equality
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Recursion
-----------------------------------------------------------------------------|#

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (sum-to n)
  (if (= n 0)
      0
      (+ n (sum-to (- n 1)))))

(define (fractal img depth)
  (if (= depth 0)
    img
    (beside img
            (rotate 15 (fractal (scale 0.8 img) (- depth 1))))))

(define (loop i)
  (when (> i 0)
    (println "hello")
    (loop (- i 1))))

(define (yell s n)
  (let loop ([i 0])
    (when (< i n)
      (println s)
      (loop (+ i 1)))))


#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (rest lst)))))

(define (concat lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (first lst1) (concat (rest lst1) lst2))))

(define (flatten lst)
  (if (null? lst)
      '()
      (if (list? (first lst))
          (concat (flatten (first lst)) (flatten (rest lst)))
          (cons (first lst) (flatten (rest lst))))))


#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.
-----------------------------------------------------------------------------|#

(define (all? p lst)
  (cond [(null? lst) #t]
        [(not (p (first lst))) #f]
        [else (all? p (rest lst))]))

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (first lst)) (map f (rest lst)))))

(define (filter p lst)
  (if (null? lst)
      '()
      (if (p (first lst))
          (cons (first lst) (filter p (rest lst)))
          (filter p (rest lst)))))

(define (fold f init lst)
  (if (null? lst)
      init
      (fold f (f init (first lst)) (rest lst))))


#|-----------------------------------------------------------------------------
;; Even more special forms

- `apply`: apply a function to a list of arguments
- `compose`: returns a function that composes the given functions
- `curry`: returns a version of a function that can be partially applied
- `eval`: evaluate a sexp
-----------------------------------------------------------------------------|#

(apply + '(1 2 3))

(define (sum . xs)
  (apply + xs))

((compose sqrt abs) -2)

(((curry cons) 1) 2)

((curry cons 1) 2)

((((curry (lambda (x y z) (+ x y z))) 1) 2) 3)

(eval '(+ 1 2 3))


#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used
-----------------------------------------------------------------------------|#

(define (make-adder n)
  (lambda (x) (+ x n)))

(define a (make-adder 1))

(a 20)

(define x 1000)

(a 20)
