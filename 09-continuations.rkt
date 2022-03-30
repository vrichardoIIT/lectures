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

And f could *call this continuation* with the result of its application to x.
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


;; to convert these to CPS, each function needs to take the continuation as
;; an additional argument (`k` is traditionally used as the name of the
;; continuation function, but try naming it `return`!)

(define (k2c& t k) (k (- t 273.15)))

(define (c2f& t k) (k (+ (* t 9/5) 32)))

(define (f2h& t k)
  (k (cond [(< t 32) "Brrrrrr..."]
           [(> t 90) "Hot!"]
           [else "Comfy"])))

(define (k2h& t k)
  (k2c& t  (lambda (t1)  ; see how CPS explicitly encodes control flow!
  (c2f& t1 (lambda (t2)
  (f2h& t2 (lambda (t3)  ; if you squint at this, it kinda looks imperative
  (k t3)))))))) ; especially if you rename k "return"

(trace k2c c2f f2h k2c& c2f& f2h&)

#; (k2h 300)
#; (k2h& 300 identity)


;; write the standard recursive `sum` in CPS:

(define (sum lst)
  (if (empty? lst)
      0
      (+ (first lst) (sum (cdr lst)))))
  

(define (sum& lst k)
  (if (empty? lst)
      (k 0)
      (sum& (cdr lst)
            (lambda (v)
              (k (+ (first lst) v))))))

(trace sum sum&)

#; (sum (range 10))
#; (sum& (range 10) identity)


#|-----------------------------------------------------------------------------
;; CPS as tail-call everything

In CPS, functions *never return*; they always pass their results to another
function (the continuation). Since this happens in the tail position, CPS -- if
implemented thoroughly, and with TCO -- makes it unnecessary to maintain a
function (control) stack.

Does this mean CPS converts all O(N)-space programs into O(1)?
-----------------------------------------------------------------------------|#


;; let's rewrite sum& from before, with + in CPS style, too

(define (+& x y k)
  (k (+ x y)))

(define (sum2& lst k)
  (if (empty? lst)
      (k 0)
      (sum2& (cdr lst)
            (lambda (v)
              (+& (first lst) v k)))))

(trace +& sum2&)

#; (sum2& (range 10) identity)


;; Even though the control stack *is* eliminated, the continuation itself grows
;; (as the "remaining" part of the program increases in size). By the last call
;; to `sum2&`, we have an unevaluated function (a "thunk") of O(N) size!


;; to fully write a program in CPS, *all* functions must be in CPS ...

(define (empty?& lst k) (k (empty? lst)))

(define (cdr& lst k) (k (cdr lst)))


;; can we write an HOF to convert an argument function into CPS?

(define (cps-ify f)
  (lambda args
    (let* ([rargs (reverse args)]
           [k (first rargs)]
           [args (reverse (rest rargs))])
      (k (apply f args)))))

(define *& (cps-ify *))
(define -& (cps-ify -))

#; (*& 2 2 (lambda (v)
   (-& 3 v identity)))


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
 ontrol in a program, have many other applications:

- exceptions
- (lazy) generators
- coroutines
- asynchronous communication
-----------------------------------------------------------------------------|#

;; consider a CPS implementation of / (division) which wishes to explicitly
;; encode two possible continuations: normal and error (div by 0)

(define (/& x y k e)
  (if (= y 0)
      (e "Div by zero!")
      (k (/ x y))))

#; (/& 10 2 identity error)
#; (/& 10 0 identity error)


;; we can use this to short-circuit a function when an exception is detected

(define (sum3& lst k e)
  (cond [(empty? lst) (k 0)]
        [(number? (first lst))
         (sum3& (cdr lst)
                (lambda (v)
                  (+& (first lst) v k))
                e)]
        [else
         (e (format "~a is not a number" (first lst)))])) ; like a jump/goto!

(trace sum3&)

#; (sum '(1 2 3 boom 4 5 6))
#; (sum2& '(1 2 3 boom 4 5 6) identity)
#; (sum3& '(1 2 3 boom 4 5 6) identity error)