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
(apply + '(1 2 3 4))

(apply + 1 2 '(3 4 ))

(apply cons '( 1 2 ))

(define (sum1 . xs)
  (apply + xs))


(define (sum . ns) ; the dot indicates that ns is a rest argument (takes argument of a list and "unpacks" it)
  (* 2 (apply + ns)))

;; `curry` gives us partial application
(define conswith3 (curry cons 3)) ;takes first arg then cons with 3, '(n . 3)

(define (arity-of-3 x y z) ;(y+z) * x
  (* x (+ y z)))

(define foo (curry arity-of-3 5)) ;*y +z )* 5

(define (flip f)
  (lambda (x y) (f y x)))

(define conswith3.2 (curry (flip cons) 3));takes second arg and cons 3 with that
    
;((flip cons) 1 2 ), this will take cons as an arg then 1 = x and 2 =y , cons y x will flips anf give '(y . x)

;; compose is a simple but powerful form of "functional "glue"
((compose sqrt abs) -4) ;== (sqrt(abs -4))

(define (mycompose f g); (f . g)(x) = f(g(x))
 (lambda (x)
   (f (g x))))

(define even?
  (mycompose
   (curry = 0)
   (curry(flip remainder) 2)))
 

;; eval is like having access to the Racket compiler in Racket!
(define (myif test e1 e2) ;test will be the condition in list form, e1 will be like true, e2 will be false
 (eval `(cond (,test, e1) ;coma to evaluate
         (else, e2))))

(define (3times e)
  (eval `(begin ,e ,e ,e))) ;the 'begin' keyword is use to put stuff together.



#|-----------------------------------------------------------------------------
;; Some list-processing HOFs

- `map`: applies a function to every element of a list, returning the results

- `filter`: collects the values of a list for which a predicate tests true

- `foldr`: implements *primitive recursion*

- `foldl`: like `foldr`, but folds from the left; tail-recursive
-----------------------------------------------------------------------------|#
;map
(define (map f l)
  (if (empty? l)
      '()
      (cons (f (first l));construct a new list, where each element is the result of applying f to an element in l
            (map f (rest l)))))
      


;; `map` examples
 (values
   (map add1 (range 10))

   (map (curry * 2) (range 10))

   (map string-length '("hello" "how"  "is" "the" "weather?")))

;filter
(define (filter p l)
  (cond [(empty? l) '()]
        [(p (first l)) (cons (first l)
                             (filter p (rest l)))]
        [else (filter p (cons (rest l)))]))

;; `filter` examples
#; (values 
   (filter even? (range 10))
   
   (filter (curry < 5) (range 10))

   (filter (compose (curry equal? "hi")
                    car)
           '(("hi" "how" "are" "you")
             ("see" "you" "later")
             ("hi" "med" "low")
             ("hello" "there"))))
;foldr (reduce)
(define (foldr f val lst) ;f = a function, val = base case, lst = lst
  (if (empty? lst)
      val
      (f (first lst) (foldr f val (rest lst)))))
  
;looking at function sum2 and copylst we can see that foldr here iss just a generalize version of those function
#| visualization of foldr sum
(cons 0(cons 1(cons 2 (cons 3 '())))), '() is the basecase
it will then replace '() with 0 and cons with + as we recurse back!


|#
;; `foldr` examples (fold right)
#; (values
    (foldr + 0 (range 10))

    (foldr cons '() (range 10))

    (foldr cons '(a b c d e) (range 5))

    (foldr (trace-lambda (x acc) (cons x acc)) ; try trace-lambda
           '() (range 5)))

#;(define (sum2 lst)
  (if (empty? lst)
      0
      (+ (first lst) (sum2 (rest lst)))))

(define sum2 (curry foldr + 0))

(trace sum2)

#;(define (foo2 lst) ;basically a copy function in this imp
  (if (empty? lst)
      '()
      (cons (first lst) (foo2 (rest lst)))))

(define copylst (curry foldr cons '()))

(define (concatenate l1 l2)
  (foldr cons l2 l1))

;foldl
(define (foldl f acc lst)
  (if (empty? lst)
      acc
      (foldl f (f (first lst) acc) (rest lst))))




;; `foldl` examples (fold left)
#; (values
    (foldl + 0 (range 10))
    
    (foldl cons '() (range 10))
    
    (foldl cons '(a b c d e) (range 5))
    
    (foldl (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))

(define sum3 (curry foldl + '()))
(define reverse (curry foldl cons  '()))


(define (partition x lst)
  (foldl (lambda (y acc)
           (if (< y x)
               (list (cons y (first acc))
                     (second acc))
               (list (first acc)
                     (cons y (second acc)))))
         '(() ())
         lst))
 

#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used

- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#

(define (simple n)
  (let ([loc 10])
    (+ n loc)))

(define (weird n)
  (let ([loc n])
    (lambda ()
      (println loc))))

(define (weird2 n)
  (lambda ()
    (println n)))

(define (make-adder n)
  (lambda (x) (+ x n)))

(define (make-obj) 
  (let  ([attr 0]) ;attirbute
    (lambda (cmd) ;command
      (case cmd
        ['inc (set! attr (add1 attr))] ;command 1 
        ['dec (set! attr (sub1 attr))] ;kinda like method calls 
        ['show (println attr)])))) ;kinda like getter

#|
functions can hang on to a value that is given to them, continue to use them. kinda like an object.

life time of a varibale is tied to the function that is refering back to them
the function closes over the variables


|#

