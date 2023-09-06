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

(trace sum-to)


;; This version uses an accumulator to avoid doing work after the recursive
;; call returns. This lets Racket performs tail-call optimization (TCO).
(trace-define (sum-to-acc n acc)
  (if (= n 0)
      acc
      (sum-to-acc (sub1 n) (+ n acc))))


;; Can use `letrec` to define local recursive functions.
(define (sum-to-acc-2 n)
  (letrec ([sum (trace-lambda (i acc)
                  (if (= i 0)
                      acc
                      (sum (sub1 i) (+ acc i))))])
    (sum n 0)))


;; Alternatively, can use `define` to define a local function
(define (sum-to-acc-3 n)
  (trace-define (sum i acc)
    (if (= i 0)
        acc
        (sum (sub1 i) (+ acc i))))
  (sum n 0))
      


;; Can also use alternative form of `let`.
(define (sum-to-acc-4 n)
  (trace-let sum ([i n]
                  [acc 0])
    (if (= i 0)
        acc
        (sum (sub1 i) (+ acc i)))))


;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(trace-define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


;; using an accumulator
(define (fib-tail n)
  (trace-let rec ([f0 0]
                  [f1 1]
                  [i 0])
    (if (= i n)
        f0
        (rec f1 (+ f0 f1) (add1 i)))))



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(trace-define (length lst)
  (if (empty? lst)
      0
      (+ 1 (length (rest lst)))))


(trace-define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))


(trace-define (concat l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (car l1)
                    (concat (cdr l1) l2))]))


(trace-define (reverse lst)
  (if (empty? lst)
      lst
      (concat (reverse (cdr lst))
              (list (car lst)))))


(define (reverse-tail lst)
  (trace-let rec ([lst lst]
                   [acc '()])
    (if (empty? lst)
        acc
        (rec (rest lst) (cons (first lst) acc)))))


(define (range n)
  (trace-let rec ([i 0]
                  [acc '()])
    (if (= i n)
        (reverse-tail acc)
        (rec (+ i 1) (cons i acc)))))