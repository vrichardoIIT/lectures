#lang racket

(require racket/trace)

#|-----------------------------------------------------------------------------
;; Continuations and Continuation Passing Style (CPS)

While in the middle of evaluating an expression, the *continuation* represents
the rest of the expression to be evaluated.

Consider the following expression:

    (h (g (f x)))

After evaluating (f x), we might represent the continuation as:

    (h (g [[ ]]))

Where the semantic brackets ([[ ]]) represent where evaluation picks back up.

Normally, (f x) would return a result, which would get picked up by the
continuation, and evaluation would proceed as usual. We call this the "direct
style" of programming.

Alternatively, we could represent the continuation as a function:

    (lambda (v) (h (g v)))

And f could *call this continuation* with the result of its application to x:

    (f x (lambda (v) (h (g v))))

This makes the flow of control from f to its continuation explicit. We call
this *continuation passing style* (CPS).

-----------------------------------------------------------------------------|#

;; some temperature conversion functions written in direct style

(define (k2c t) (- t 273.15))

(define (c2f t) (+ (* t 9/5) 32))

(define (f2h t)
  (cond [(< t 32) "Brrrrrr..."]
        [(> t 90) "Hot!"]
        [else "Comfy"]))

(define (k2h t)
  (f2h (c2f (k2c t))))


;; convert the above to CPS



;; write the standard recursive `sum` in CPS:

(define (sum lst)
  (if (empty? lst)
      0
      (+ (first lst) (sum (cdr lst)))))



#|-----------------------------------------------------------------------------
;; CPS as tail-call everything

In CPS, functions *never return*; they always pass their results to another
function (the continuation). Since this happens in the tail position, CPS -- if
implemented thoroughly, and with TCO -- makes it unnecessary to maintain a
function (control) stack.

Does this mean CPS converts all O(N)-space programs into O(1)?
-----------------------------------------------------------------------------|#


;; let's rewrite sum& from before, with + in CPS style, too



;; to fully write a program in CPS, *all* functions must be in CPS ...



;; can we write an HOF to convert an argument function into CPS?

(define (cps-ify f)
  void)

(define *& (cps-ify *))
(define -& (cps-ify -))


#|-----------------------------------------------------------------------------
;; Applications of Continuations

CPS is infrequently used (especially globally) by programmers, but it can be
used as an intermediate representation for compilers of functional languages.

CPS demonstrates a technique also known as the *callback function*. This is
often used in situations where we aren't sure when the result of some function
will be available, and so we pass a callback/continuation which will be used
to "call back" with the result.

E.g., consider the following Node.js (an asynch JavaScript framework) code:

    function processData (callback) {
      fetchData(function (err, data) {
        if (err) {
          console.log("An error has occurred. Abort everything!");
          return callback(err);
        }
        data += 1;
        callback(data);
      });
    }

---

Continuations, as a general mechanism for explicitly manipulating the flow of
control in a program, have many other applications:

- exceptions
- (lazy) generators
- coroutines
- asynchronous communication
-----------------------------------------------------------------------------|#

;; consider a CPS implementation of / (division) which wishes to explicitly
;; encode two possible continuations: normal and error (div by 0)



;; we can use this to short-circuit a function when an exception is detected

