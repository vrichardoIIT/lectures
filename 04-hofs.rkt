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
#; (values
    (apply + '(1 2 3))
    (apply + 1 2 '(3))
    (apply + 1 2 3 '())) ; the last argument to `apply` has to be a list

(define (sum . xs)
  (apply + xs))

;; `curry` gives us partial application
#; (values
    (cons 1 2)
    (curry cons 1 2)
    ((curry cons) 1 2)
    (((curry cons) 1) 2)
    ((curry cons 1) 2))

#; (((curry (lambda (x y z) (+ x y z)) 1) 2) 3)

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))

(define thrice (curry repeat 3))

;; compose is a simple but powerful form of "functional "glue"
#; ((compose sqrt abs) -2)

(define (flip f)
  (lambda (x y) (f y x)))

(define even?
  (compose (curry = 0)
           (curry (flip remainder) 2)))

;; eval is like having access to the Racket compiler in Racket!
#; (values
    (eval '(+ 1 2 3))
    (eval (cons 'println (cons "hello" '()))))

(define (my-if test e1 e2)
  (eval `(cond (,test ,e1)
               (else ,e2))))

#; (my-if '(< 1 2) '(println "true") '(println "false"))

(define (repeatedly n sexp)
  (eval (cons 'begin (repeat n sexp))))

#; (repeatedly 10 '(println "hello"))


#|-----------------------------------------------------------------------------
;; Some list-processing HOFs
-----------------------------------------------------------------------------|#

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (first lst)) (map f (rest lst)))))

#; (map (curry * 2) (range 10))
#; (map (lambda (r) (circle r "solid" "blue")) (range 10))

(define (filter p lst)
  (if (null? lst)
      '()
      (if (p (first lst))
          (cons (first lst) (filter p (rest lst)))
          (filter p (rest lst)))))

#; (filter even? (range 10))
#; (filter (curry < 5) (range 10))

(define (foldr f init lst)
  (if (null? lst)
      init
      (f (first lst) (foldr f init (rest lst)))))

#; (trace foldr)
#; (foldr + 0 (range 10))
#; (foldl - 0 (range 10))
#; (foldr / 1 '(2 3 4))

(define (foldl f init lst)
  (if (null? lst)
      init
      (foldl f (f init (first lst)) (rest lst))))

#; (trace foldl)
#; (foldl + 0 (range 10))
#; (foldl - 0 (range 10))
#; (foldl / 1 '(2 3 4))


#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used
- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#

(define (make-adder n)
  (lambda (x) (+ x n)))

(define a (make-adder 1))

#; (a 20)

(define x 1000)

#; (a 20)