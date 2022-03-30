#lang racket

#|-----------------------------------------------------------------------------
;; First-class continuations

In CPS, we manually create continuations as functions and pass/invoke them.
If we neglect to pass a continuation into a function, the latter has no way of
obtaining the current continuation.

In a language with support for *first-class continuations*, the current
continuation can be obtained (but not necessarily used) at any point. This
greatly simplifies and encourages the use of continuations, and enables many
different uses cases!

In Racket we have the function `call-with-current-continuation`, aka `call/cc`.
-----------------------------------------------------------------------------|#

;; write (* 5 (+ 2 3)) in CPS with call/cc
(* 5 (call/cc (lambda (k) (k (+ 2 3)))))


;; we can save a continuation!
(define cc void)

(define (foo x)
  (* x (call/cc (lambda (k)
                  (set! cc k)
                  0))))

#; (foo 20)
#; (cc 1)
#; (cc 11)
#; (foo 30)
#; (cc 11)

(define (save-cc! x)
  (call/cc (lambda (k)
             (set! cc k)
             x)))

(define (bar x y z)
  (+ x (/ (+ (save-cc! 0) y) z)))

#; (bar 10 5 10)
#; (cc 25)


;; Continuations are like the "goto" of functional programming!


;; what does this function do?
(define (bat)
  (let ([cc (call/cc (lambda (k) k))])
    (cc cc)))

(define (label)
  (call/cc (lambda (k) k)))

(define (goto lbl)
  (lbl lbl))

(define (print-range n)
  (define x 0)
  (let ([loop (label)])
    (println x)
    (set! x (add1 x))
    (when (< x 10) (goto loop))))


#|-----------------------------------------------------------------------------
;; Aside: dynamic bindings vs. set!

`make-parameter` and `parameterize`
-----------------------------------------------------------------------------|#


#|-----------------------------------------------------------------------------
;; Continuations for exceptional control flow


-----------------------------------------------------------------------------|#
