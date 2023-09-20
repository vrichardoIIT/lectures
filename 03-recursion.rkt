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
;note ^: space complexity, how many stack frames. in this case o(n) 

(trace-define (sum-to-acc n acc) ; acc =  accumulator
  (if (= n 0)
      acc
      (sum-to-acc (sub1 n)(+ n acc))))
;note ^: output will not have indentation, meaning no stack frames, this is beacuse we are calling recursion in tail position
#|
(define (sum-to-acc-2 n)
  (trace-define (sum i acc)
   (if (= i 0 )
      acc
      (sum(sub1 i)(+ acc i))))
   (sum n 0))


  |#

;Tail-call optimization




;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(trace-define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fib-tail n)
              (trace-let rec ([f0 0]
                        [f1 1]
                        [i 0])
                (if (= i n)
                    f0
                    (rec f1 (+ f1 f0) (add1 i)))))

; (trace fib)



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(trace-define (length lst)
  (if(empty? lst)
     0
     (add1 (length (rest lst)))))


(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (sub1 n) x ))))

(trace-define (concat l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1)
                    (concat (rest l1) l2))]))


(define (reverse lst)
  (if (empty? lst)
      lst
      (concat (reverse (rest lst))(list(first lst)))))


(define (range n)
  (void))