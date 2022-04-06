#lang racket

(require racket/trace
         macro-debugger/stepper)

#|-----------------------------------------------------------------------------
;; First-class continuations

In CPS, we manually create continuations as functions and pass/invoke them.
If we neglect to pass a continuation into a function, the latter has no way of
obtaining the current continuation.

In a language with support for *first-class continuations*, the current
continuation can be obtained (but not necessarily used) at any point. This
greatly simplifies and encourages the use of continuations, and enables many
different uses cases!

In Racket we have the function `call-with-current-continuation`, aka `call/cc`,
which takes as an argument another function `proc`. When `call/cc` is called,
it captures the current continuation k, which is passed to `proc`. If `k` is
called, its argument is passed (in tail position) to the continuation, else
the result of `proc` is the result of `call/cc`.
-----------------------------------------------------------------------------|#

;; evaluate & explain:

; (* 10 (call/cc (lambda (_) 20)))

; (* 10 (call/cc (lambda (k) (k 20))))

; (* 10 (call/cc (lambda (k) (k (* 2 10)))))

; (* 10 (call/cc (lambda (k) (* 2 (k 10)))))


;; we can save a continuation!
(define *k* void)

(define (arith x)
  (* x (call/cc (lambda (k)
                  (set! *k* k)
                  (k 0)))))

; (arith 10)
; (*k* 1)
; (*k* 11)
; (arith 20)
; (*k* 11)


(define (save-cc! x)
  (call/cc (lambda (k)
             (set! *k* k)
             (k x))))

(define (arith2 x y z)
  (+ x (/ (+ (save-cc! 0) y) z)))

; (arith2 10 5 10)
; (*k* 25)


;; continuations are dynamic, not lexical!
(define (sum-from n)
  (if (= n 0)
      (save-cc! 0)
      (+ n (sum-from (sub1 n)))))

; (sum-from 10)
; (*k* 10)


#; ; the extent of continuations can be controlled
(* 10 (+ 100
         (call-with-continuation-prompt ; creates a continuation "ceiling"
          (lambda ()
            (+ 5 (* 3
                    (save-cc! 0)))))))

; (*k* 10)


#|-----------------------------------------------------------------------------
;; Continuations as a functional "goto"
-----------------------------------------------------------------------------|#

;; what does this function do?
(define (foo)
  (let ([kk (call/cc (lambda (k) (k k)))])
    (kk kk)))


;; we can define some utility functions
(define (current-continuation)
  (call/cc (lambda (k) (k k))))

(define (goto k)
  (k k))

(define (print-range n)
  (let* ([x 0]
         [loop (current-continuation)])
    (println x)
    (set! x (add1 x))
    (when (< x 10)
      (goto loop))))


#|-----------------------------------------------------------------------------
;; ... as an escape hatch
-----------------------------------------------------------------------------|#

;; short-circuit break/return
(define (find pred lst)
  (call/cc 
   (lambda (return)
     (for ([x lst])
       (when (pred x)
         (return x))))))

; (find (curry = 5) (range 10))


;; non-local exit!
(define (find2 pred lst)
  (call/cc 
   (lambda (return)
     (for ([x lst])
       (pred x return)))))

#; (find2 (lambda (x return)
            (when (= x 5) (return x)))
          (range 10))


#|-----------------------------------------------------------------------------
;; ... for implementing co-routines
-----------------------------------------------------------------------------|#

(define (ping k n)
  (let loop ([i 1])
    (println (format "ping ~a" i))
    (set! k (call/cc k)) ; each time back, k is a continuation in pong!
    (when (< i n)
      (loop (add1 i)))))

(define (pong k)
  (let loop ()
    (println "pong")
    (set! k (call/cc k))
    (loop)))

(trace ping pong)

;; more realistically
(define (long-computation k)
  (println "Phase 1")
  (sleep 0.5)
  (set! k (call/cc k))
  (println "Phase 2")
  (sleep 0.5)
  (set! k (call/cc k))
  (println "Phase 3")
  (sleep 0.5)
  (set! k (call/cc k))
  (sleep 0.5)
  (println "Done"))

(define (periodic-task k)
  (let loop ([x 0])
    (println (format "Periodic task ~a" x))
    (sleep 0.5)
    (set! k (call/cc k))
    (loop (add1 x))))

; (long-computation periodic-task)


;; even better, write a function that cycles between different continuations
;; to resume executing --- an implementation of lightweight "threads"


#|-----------------------------------------------------------------------------
;; ... for implementing exceptions and exception handling
-----------------------------------------------------------------------------|#

(define *estack* '())

(define-syntax (try stx)
  (syntax-case stx (catch)
    [(_ body ... catch id handler)
     #'(let ([exception (current-continuation)])
         (if (continuation? exception)
             (begin
               (set! *estack* (cons exception *estack*))
               body ... 
               (set! *estack* (cdr *estack*))) ; may not get here
             ((lambda (id) handler) exception)))]))

(define (throw e)
  (let ([k (car *estack*)])
    (set! *estack* (cdr *estack*)) ; not very elegant!
    (k e)))

#;
(try (println "L1")
     (println "L2")
     (println "L3")
 catch e
     (println (format "Exception: ~a" e)))

#; 
(try (println "L1")
     (println "L2")
     (throw 'BANG)
     (println "L3")
 catch e
     (println (format "Exception: ~a" e)))

#;
(try
     (try (println "L1")
          (println "L2")
          (throw 'BANG)
          (println "L3")      
      catch e
          (begin
            (println (format "Inner: ~a" e))
            (throw 'BOOM)))
 catch e
     (println (format "Outer: ~a" e)))

;; NB: for real implementations, better to use dynamic-wind or dynamic binding