#lang racket

(require racket/trace) ; for dynamic function call traces


#|-----------------------------------------------------------------------------
;; Recursion (and Iteration)
-----------------------------------------------------------------------------|#

(define (println-times datum n)
  (when (> n 0)
    (println datum)
    (println-times datum (sub1 n))))


;; integer summation
(define (sum-to n)
  (if (= n 0)
      0
      (+ n (sum-to (sub1 n)))))
; (trace sum-to)

;; This version uses an accumulator to avoid doing work after the recursive
;; call returns. This lets Racket performs tail-call optimization (TCO).
(define (sum-to-acc n acc)
  (if (= n 0)
      acc
      (sum-to-acc (sub1 n) (+ acc n))))
; (trace sum-to-acc)

;; Can use `letrec` to define local recursive functions.
(define (sum-to-acc-2 n)
  (letrec ([sum (lambda (i acc) ; can use trace-lambda for tracing
                  (if (= i 0)
                      acc
                      (sum (sub1 i) (+ acc i))))])
    (sum n 0)))

;; Alternatively, can use `define` to define a local function
(define (sum-to-acc-3 n)
  (define (sum i acc)
    (if (= i 0)
        acc
        (sum (sub1 i) (+ acc i))))
  ; (trace sum)
  (sum n 0))

;; Can also use alternative form of `let`.
(define (sum-to-acc-4 n)
  (let sum ([i n] ; can use trace-let for tracing
            [acc 0])
    (if (= i 0)
        acc
        (sum (sub1 i) (+ acc i)))))


;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
; (trace fib)

;; using an accumulator
(define (fib-tail n)
  (let rec ([f0 0]
            [f1 1]
            [m 0])
    (if (= m n)
        f0
        (rec f1 (+ f0 f1) (add1 m)))))



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