#lang racket

(require racket/trace)


#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.

Some useful built-in HOFs and related functions:

- `apply`: apply a function to a list of arguments

- `curry`: returns a version of a function that can be partially applied

- `compose`: returns a function that is the composition of two other functions

- `eval`: evaluates a sexp
-----------------------------------------------------------------------------|#

;; `apply` applies a function to lists
(values
 (apply + '(1 2 3))
 (apply + 1 2 '(3))
 (apply + 1 2 3 '())) ; the last argument to `apply` has to be a list

(define (sum . xs)
  (apply + xs))


;; `curry` gives us partial application
(values
 (cons 1 2)
 (curry cons 1 2)
 ((curry cons) 1 2)
 (((curry cons) 1) 2)
 ((curry cons 1) 2))

(((curry (lambda (x y z) (+ x y z)) 1) 2) 3)

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))

(define thrice (curry repeat 3))


;; compose is a simple but powerful form of "functional "glue"
((compose sqrt abs) -2)

(define (my-compose f g)
  (lambda (x) (f (g x))))

(define (flip f)
  (lambda (x y) (f y x)))

(define even?
  (my-compose (curry = 0)
              (curry (flip remainder) 2)))


;; eval is like having access to the Racket compiler in Racket!
(values ; note -- no need for explicit namespace at REPL
 (eval '(+ 1 2 3) (make-base-namespace))
 (eval (cons 'println (cons "hello" '())) (make-base-namespace)))

(define (my-if test e1 e2)
  (eval `(cond (,test ,e1)
               (else ,e2))))

; (my-if '(< 1 2) '(println "true") '(println "false"))

(define (repeatedly n sexp)
  (eval (cons 'begin (repeat n sexp))))

; (repeatedly 10 '(println "hello"))


#|-----------------------------------------------------------------------------
;; Some list-processing HOFs

- `map`: applies a function to every element of a list, returning the results

- `filter`: collects the values of a list for which a predicate tests true

- `foldr`: implements *primitive recursion*

- `foldl`: like `foldr`, but folds from the left; tail-recursive
-----------------------------------------------------------------------------|#

;; `map`
(define (map f lst)
  (if (empty? lst)
      '()
      (cons (f (first lst)) (map f (rest lst)))))


(values
 (map add1 (range 10))

 (map (curry * 2) (range 10))
 
 (map string-length '("hello" "how" "is" "the" "weather?")))


;; `filter`
(define (filter p lst)
  (cond [(empty? lst) '()] ;if lst is empty just return the empty lst
        [(p (first lst)) (cons (first lst) (filter p (rest lst)))] ;apply test on first element if true cons first element and recurse true the rest 
        [else (filter p (rest lst))])) ;if false then just recurse thru the rest


(values 
 (filter even? (range 10))
   
 (filter (curry < 5) (range 10))

 (filter (compose (curry equal? "hi")
                  car)
         '(("hi" "how" "are" "you")
           ("see" "you" "later")
           ("hi" "med" "low")
           ("hello" "there"))))


;; `foldr`
(define (foldr f val lst)
  (if (empty? lst)
      val
      (f (first lst) (foldr f val (rest lst)))))

; (trace foldr)

(define sum2 (curry foldr + 0))

(define copy-list (curry foldr cons '()))

(define (concat l1 l2) (foldr cons l2 l1))


(values
 (foldr + 0 (range 10))

 (foldr cons '() (range 10))

 (foldr cons '(a b c d e) (range 5))

 (foldr (lambda (x acc) (cons x acc))
        '()
        (range 5)))


;; `foldl`
(define (foldl f acc lst)
  (if (empty? lst)
      acc
      (foldl f (f (first lst) acc) (rest lst))))

(trace foldl)

(define sum3 (curry foldl + 0))

(define reverse (curry foldl cons '()))

(define (partition x lst)
  (foldl (lambda (y acc)
           (if (< y x)
               (list (cons y (first acc))
                     (second acc))
               (list (first acc)
                     (cons y (second acc)))))
         '(() ())
         lst))


(values
 (foldl + 0 (range 10))
    
 (foldl cons '() (range 10))
    
 (foldl cons '(a b c d e) (range 5))
    
 (foldl (lambda (x acc) (cons x acc))
        '()
        (range 5)))


;; for/fold is a handy built-in function for accumulating results a la fold
;; - basic syntax: (for/fold ([accum-id init-expr])
;;                           ([id seq-expr] ...)
;;                    body)

(for/fold ([sum 0])
          ([n (range 11)])
  (+ n sum))

(for/fold ([acc '()])
          ([val '(a b c d e)])
  (cons val acc))

(for/fold ([acc '()])
          ([v1 (range 10)]
           [v2 '(a b c d e)])
  (cons (cons v2 v1) acc))

(for/fold ([h (hash)])
          ([name '("Molly" "Eric" "Anna" "Sam" "Rishi")])
  (hash-set h name (string-length name)))



#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used

- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#

(define (simple n)
  (let ([loc 10]) ; what's the lifetime of this local var?
    (+ n loc)))

(define (weird n)
  (let ([loc n]) ; how about this one?
    (lambda ()
      (println loc))))

(define foo (weird 10))

(foo)

(define (weird2 n)
  (lambda ()
    (println n))) ; params are also local vars

(define foo2 (weird2 20))

(foo2)

(define (make-adder n)
  (lambda (x) (+ x n))) ; functions "close" over their data

(define a (make-adder 1))

(a 10)
(a 20)

(define (make-obj) ; objects are special cases of closures!
  (let ([attr 0])
    (lambda (cmd)
      (case cmd
        ['up (set! attr (add1 attr))]
        ['down (set! attr (sub1 attr))]
        ['show (println attr)]))))

(define o (make-obj))
(o 'show)
(o 'up)
(o 'up)
(o 'up)
(o 'show)
(o 'down)
(o 'show)