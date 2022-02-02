#lang racket

(require racket/trace ; for dynamic function call traces
         2htdp/image) ; for a bit of fun with pictures

(define *car-wheel* (circle 20 "solid" "grey"))

(define *car-body* (beside/align "bottom"
                    (square 40 "solid" "teal")
                    (square 70 "solid" "teal")
                    (square 40 "solid" "teal")))

(define *car* (let ([wheels (beside *car-wheel*
                                    (rectangle 40 0 "solid" "white")
                                    *car-wheel*)])
                (overlay/offset wheels
                                0 -30
                                *car-body*)))


#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions
-----------------------------------------------------------------------------|#

#; ((lambda (x y z)
      (* x (+ y z)))
    2 3 4)

#; ((lambda (x y z)
      (println x)
      (println y)
      (println z)
      (* x (+ y z)))
    2 3 4)

#; (let ([f (lambda (x y z) 
              (* x (+ y z)))])
     (f 2 3 4))

(define foo (lambda (x y z) 
              (* x (+ y z))))

(define (foo2 x y z)
  (* x (+ y z)))

(define (bar x y . z) ; `z` is a a list "rest" argument
  (println x)
  (println y)
  (println z))


#|-----------------------------------------------------------------------------
;; Some more special forms

- `begin`: sequences multiple sexps; evaluates to the result of the last
- `if`: if-then-else
- `when`: if-then
- `cond`: multi-way conditional
- `match`: pattern match
-----------------------------------------------------------------------------|#

#; (begin body ...)

#; (if test-expr then-expr else-expr)

#; (when test-expr body ...)

#; (cond [expr1 body ...]
         [expr2 body ...]
         [else body ...])

#; (match val
     [pattern1 body ...]
     [pattern2 body ...]
     [pattern3 body ...])
     


#|-----------------------------------------------------------------------------
;; Equality tests

- `=` for numbers
- `eq?` for pointer-based equality
- `equal?` for value-based equality
-----------------------------------------------------------------------------|#

#; (values 
     (= 2 2)
     (= 2 2.0)
     (= 2 2.01)
     (= 2 2.0000000000000000001)
     (= 2 8/4)
     (eq? 'a 'a)
     (eq? "hello world" "hello world")
     (eq? '(a b c) '(a b c))
     (let ([lst '(a b c)]) (eq? lst lst))
     (equal? "hello world" "hello world")
     (equal? '(a b c) '(a b c)))


#|-----------------------------------------------------------------------------
;; Recursion
-----------------------------------------------------------------------------|#

;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
#; (trace fib)

(define (sum-to n)
  (if (= n 0)
      0
      (+ n (sum-to (- n 1)))))
#; (trace sum-to)

;; This version uses an accumulator to avoid doing work after the recursive
;; call returns. This lets Racket performs tail-call optimization (TCO).
(define (sum-to-acc n sum)
  (if (= n 0)
      sum
      (sum-to-acc (- n 1) (+ sum n))))
#; (trace sum-to-acc)

;; Can use `letrec` to define local recursive functions.
(define (sum-to-acc-2 n)
  (letrec ([rec (lambda (i sum) ; can use trace-lambda for tracing
                  (if (= i 0)
                     sum
                     (rec (- i 1) (+ sum i))))])
    (rec n 0)))

;; Can also use alternative form of `let`.
(define (sum-to-acc-3 n)
  (let rec ([i n] ; can use trace-let for tracing
            [acc 0])
    (if (= i 0)
        acc
        (rec (- i 1) (+ acc i)))))

;; Recursion *is* iteration.
(define (loop i)
  (when (> i 0)
    (println "hello")
    (loop (- i 1))))

(define (yell n str)
  (let loop ([i 0])
    (when (< i n)
      (println str)
      (loop (+ i 1)))))


#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (if (empty? lst)
      0
      (+ 1 (length (rest lst)))))

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))

(define (reverse lst)
  (let rec ([lst lst]
            [acc '()])
    (if (empty? lst)
        acc
        (rec (rest lst) (cons (first lst) acc)))))

(define (range n)
  (let rec ([i 0]
            [acc '()])
    (if (= i n)
        (reverse acc)
        (rec (+ i 1) (cons i acc)))))

(define (concat lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (first lst1) (concat (rest lst1) lst2))))


#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.

Some useful built-in HOFs and related functions:
- `apply`: apply a function to a list of arguments
- `curry`: returns a version of a function that can be partially applied
- `compose`: returns a function that is the composition of two other functions
- `eval`: evaluates a sexp
-----------------------------------------------------------------------------|#

;; `apply` applies a function to lists
#; (values
    (apply + '(1 2 3))
    (apply + 1 2 '(3))
    (apply + 1 2 3 '())) ; the last argument to `apply` has to be a list

(define (sum . xs)
  (apply + xs))

;; `curry` gives us partial application
#; (values
    (cons 1 2)
    (curry cons 1 2)
    ((curry cons) 1 2)
    (((curry cons) 1) 2)
    ((curry cons 1) 2))

#; (((curry (lambda (x y z) (+ x y z)) 1) 2) 3)

(define thrice (curry repeat 3))

;; compose is a simple but powerful form of "functional "glue"
#; ((compose sqrt abs) -2)

(define planet-with
  (compose (curry above (circle 100 "solid" "blue"))
           (curry rotate 180)
           (curry scale 0.2)))

(define (flip f)
  (lambda (x y) (f y x)))

(define even?
  (compose (curry = 0)
           (curry (flip remainder) 2)))

;; eval is like having access to the Racket compiler in Racket!
#; (values
    (eval '(+ 1 2 3))
    (eval (cons 'println (cons "hello" '()))))

(define (my-if test e1 e2)
  (eval `(cond (,test ,e1)
               (else ,e2))))

#; (my-if '(< 1 2) '(println "true") '(println "false"))

(define (repeatedly n sexp)
  (eval (cons 'begin (repeat n sexp))))

#; (repeatedly 10 '(println "hello"))


#|-----------------------------------------------------------------------------
;; Some list-processing HOFs
-----------------------------------------------------------------------------|#

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (first lst)) (map f (rest lst)))))

#; (map (curry * 2) (range 10))
#; (map (lambda (r) (circle r "solid" "blue")) (range 10))

(define (filter p lst)
  (if (null? lst)
      '()
      (if (p (first lst))
          (cons (first lst) (filter p (rest lst)))
          (filter p (rest lst)))))

#; (filter even? (range 10))
#; (filter (curry < 5) (range 10))

(define (foldl f init lst)
  (if (null? lst)
      init
      (foldl f (f init (first lst)) (rest lst))))

#; (trace foldl)
#; (foldl + 0 (range 10))
#; (foldl - 0 (range 10))
#; (foldl / 1 '(2 3 4))

(define (foldr f init lst)
  (if (null? lst)
      init
      (f (first lst) (foldr f init (rest lst)))))

#; (trace foldr)
#; (foldr + 0 (range 10))
#; (foldl - 0 (range 10))
#; (foldr / 1 '(2 3 4))


#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used
-----------------------------------------------------------------------------|#

(define (make-adder n)
  (lambda (x) (+ x n)))

(define a (make-adder 1))

#; (a 20)

(define x 1000)

#; (a 20)
