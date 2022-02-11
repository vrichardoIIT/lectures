#lang racket

(require racket/trace) ; for dynamic function call traces


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
- `case`: dispatch
- `match`: pattern match
- `set!': variable assignment (mutation)
-----------------------------------------------------------------------------|#

#; (begin body ...)

#; (if test-expr then-expr else-expr)

#; (when test-expr body ...)

#; (cond [expr1 body ...]
         [expr2 body ...]
         [else body ...])

#; (case val
     [(val ...) body ...]
     [(val ...) body ...]
     [else body ...])

#; (match val
     [pattern1 body ...]
     [pattern2 body ...]
     [pattern3 body ...])

#; (set! id expr)


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