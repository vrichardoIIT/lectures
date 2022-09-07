#lang racket

(require racket/trace) ; for dynamic function call traces


#|-----------------------------------------------------------------------------
;; Recursion (and Iteration)
-----------------------------------------------------------------------------|#

(define (println-times datum n)
  (println datum))


;; integer summation
(define (sum-to n)
  (void))

; (trace sum-to)


;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; (trace fib)



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (void))


(define (repeat n x)
  (void))


(define (reverse lst)
  (void))


(define (range n)
  (void))