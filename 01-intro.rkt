#lang racket ; <- determines the language (and syntax) used in this file


#|-----------------------------------------------------------------------------
;; Racket at a glance

- LISP/Scheme dialect

  - LISP = "LISt Processing", 
           "Lots of Insidious, Silly Parentheses", 
           "Lost In a Sea of Parentheses"

- Supports (but does not enforce) functional style

  - First class functions
  - Higher-order functions
  - Anonymous functions

- Dynamically typed but type-safe

- Pass by value with pointer semantics

- Lexically scoped

- Heap-based storage with garbage collection
-----------------------------------------------------------------------------|#


;; a bit of Racket to whet your appetite
(define (quicksort < l)
  (match l
    ['() '()]
    [(cons x xs) 
     (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
       (append (quicksort < xs-lt) 
               (list x) 
               (quicksort < xs-gte)))]))


#|-----------------------------------------------------------------------------
;; Basic syntax

The essential syntactic unit of Racket is the *S-expression*, or sexp.

A sexp is either an *atom* or a *list*.

- An atom is one of:

  - a Number 
    - e.g., 42, -23, 3.14, 5/8, 2+3i, 6.02e+23

  - a '#' prefixed value 
    - e.g., booleans #t and #f, chars #\a, #\newline)

  - a String
    - e.g., "hello", "foo\nbar"

  - a Symbol, written as an identifier made of any chars except ()[]{}|",'`;#\
    - e.g., foo, hot-enough?, <=, list->set, call/cc, bang!!!

- A *list* is a sequence of sexps (recursion!) separated by spaces, 
  enclosed by parentheses

  - [] and {} can also be used as delimiters
-----------------------------------------------------------------------------|#

;; Let's write some (syntactically) valid sexps!
;; Note: `#;` is a convenient, special "sexp comment"

#; 

#; 

#; 

#; 

#; 


#|-----------------------------------------------------------------------------
;; Evaluating sexps

- Numbers, strings, and `#` prefixed values evaluate to themselves

- Symbols are identifiers whose *bindings* are looked up in the current scope

  - If an identifier corresponds to a *variable*, its value is returned

  - Identifiers may also correspond to *special forms* (more on this later)

- For lists, if the first element evaluates to a function, that function is 
  applied to the rest of the values in the list (a la prefix form)

  - e.g., `(f x y z)` applies function `f` to the values `x`, `y`, and `z`

  - Arguments are passed *by value*; i.e., the argument sexps are evaluated,  
    then their results are passed to the function (this is different for 
    special forms!)

    - What types of semantic can we not implement with functions?
-----------------------------------------------------------------------------|#

1

2.5

#t

+

(+ 300 40)

(abs -23)

(* 2 3 4)

(= (+ 2 3) (- 6 1))

(println "hello world")

#; x

#; (+ x y)

#; (foo 1 2 3)


#|-----------------------------------------------------------------------------
;; Variables

We define global variables with `define`:

  (define course-id "CS 440")

We define local variables with `let` and `let*`:

  (let ([x 1] 
        [y 2]) 
    (+ x y))

  (let* ([x 10]
         [y (* x 2)]) ; allows prior bindings to be accessed
    (+ x y))
|#


#|-----------------------------------------------------------------------------
;; Pairs and Lists

Programmatically, lists are built out of linked pairs, where a pair is 
constructed using the `cons` function:

  (cons x y)

The functions `car` and `cdr` access the first and second slots of a pair.

  (car (cons x y)) => x
  (cdr (cons x y)) => y

A list is either:

- empty (`null`), or
- a pair where the first slot refers to an element 
  and the second slot refers to a lists a list
|#

;; build a few lists manually!


#|-----------------------------------------------------------------------------
;; Quoting

The special form `quote` can be used to prevent the normal evaluation of a 
sexp, and instead just return the value of the sexp.

`quote` also has the short form `'`:

  (quote x) == 'x
  (quote (1 2 3)) == '(1 2 3)

Quote (and related forms) make it easy to write programs that manipulate and 
write other programs!

This is possible because of the *program-data equivalence* of the Racket 
language.
|#
