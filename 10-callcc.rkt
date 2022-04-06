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
  (* x 10))


#|-----------------------------------------------------------------------------
;; Continuations as a functional "goto"
-----------------------------------------------------------------------------|#

;; what does this function do?
(define (foo)
  (let ([kk (call/cc (lambda (k) (k k)))])
    (kk kk)))


#|-----------------------------------------------------------------------------
;; ... as an escape hatch
-----------------------------------------------------------------------------|#

;; short-circuit break/return
(define (find pred lst)
  (cond [(empty? lst) (void)]
        [(pred (first lst)) (first lst)]
        [else (find pred (rest lst))]))
  

#|-----------------------------------------------------------------------------
;; ... for implementing co-routines
-----------------------------------------------------------------------------|#

(define (ping k n)
  (sleep 1)
  (let loop ([i 1])
    (println (format "ping ~a" i))
    (k ping)
    (when (< i n)
      (loop (add1 i)))))

(define (pong k)
  (sleep 1)
  (let loop ()
    (println "pong")
    (k pong 10)
    (loop)))

(trace ping pong)


#|-----------------------------------------------------------------------------
;; ... for implementing exceptions and exception handling
-----------------------------------------------------------------------------|#

(define *estack* '())

(define-syntax (try stx)
  (syntax-case stx (catch)
    [(_ body ... catch id handler)
     #'()]))