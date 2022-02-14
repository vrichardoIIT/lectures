#lang racket

(require racket/trace) ; for dynamic function call traces


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