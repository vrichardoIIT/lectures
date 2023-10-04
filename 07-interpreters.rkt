#lang racket

(require racket/trace)

#|-----------------------------------------------------------------------------
;; Prelude: user defined types with `struct`
-----------------------------------------------------------------------------|#

;; define a `widget` type
(struct widget          ; type name
  (name purpose price)  ; attributes
  #:transparent)        ; when printing a widget, show its attributes

;; we get the following functions for free:
;; - `widget`: constructor
;; - `widget?`: predicate that returns #t for widget values
;; - `widget-name`: retrieve `name` attribute
;; - `widget-purpose`: retrieve `purpose` attribute
;; - `widget-price`: retrieve `price` attribute

(define w1 (widget "wrench" "wrenching" 9.99))
(define w2 (widget "plier" "pliering" 12.99))


;; define a `doohickey` type that is a sub-type of `widget`
(struct doohickey widget (special-power) #:transparent) ;doohickey is a try of widget with a "speacial power"(new atteribute)

(define d1 (doohickey "thingamajig" "thinging" 199.99 "time travel"))
;(define d1 (doohickey 'name' 'purpose' 'price' 'extra atteribute'))

;; we can also match against structs
(define (which-widget? w)
  (match w ;pattern matching construct ((pattern matching expertion) return)
    [(widget "wrench" _ _) "It's a wrench!"] ;if w is a widget and first arg "wrench" return "its a wrench"
    [(doohickey "thingamajig" _ _ _) "It's a thingamajig!"]
    [(? widget?) "It's some sort of widget"] ;apply a predicate (T/F)
    [_ "I don't know what this is"])) ;else
;_ is used for saying an arg should be here but we dont care about it
;finds the first pattern match that works


#|-----------------------------------------------------------------------------
;; Our language

We're going to start with a very simple language and slowly add to it. Our
first iteration will support integer literals, the binary arithmetic operations
 +` and `*`, and `let`-bound variables. The syntax will mirror Racket's. 
-----------------------------------------------------------------------------|#

;; Some test cases
(define p1 "(+ 1 2)")

(define p2 "(* 2 (+ 3 4))")

(define p3 "(+ x 1)")

(define p4 "(* w (+ x y))")

(define p5 "(let ([x 10])
              (+ x 1))")

(define p6 "(let ([x 10]
                  [y 20])
              (+ x y))")

(define p7 "(let ([x 10])
              (let ([y 20])
                (+ x (let ([w 30])
                       (* w y)))))")



#|-----------------------------------------------------------------------------
;; Parser

Review: What is parsing?

- Input string (source language) => Syntax object

- A syntax object contains information about the structure of the code. Often,
  we use a *tree* as an underlying representation.

  - E.g., the code "(let ([x 10]) (+ x 1))"
          
          may be parsed to the syntax tree:

                    let
                   /   \
                  x     +
                 /     / \
                10    x   1

  - Racket sexps are a programmatic way of representing syntax trees!

- Since our syntax mirrors Racket's, we may rely on Racket's reader to parse
  our input to produce an initial syntax tree.
-----------------------------------------------------------------------------|#

;;; using Racket's reader
#; (read) ;read function knows how to read sexp syntax 
;takes a port
#; (read p1) ;error
#;(read (open-input-string p1)) ;return sexp

(define (read-string str)
  (read (open-input-string str)))


#|-----------------------------------------------------------------------------
;; Parser (continued)

- Our syntax tree doesn't currently contain much information besides tokens
  pulled directly from the input string.

- Next, we will recursively descend through the syntax tree, "decorating" its
  nodes with information that can help us expand and evaluate it.
-----------------------------------------------------------------------------|#

;;; Some types for decorating our syntax tree

;; integer value
(struct int-exp (val) #:transparent) ;val is the interger value

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent) ;op (+/*) lhs = 1st arg and rhs = 2nd arg e.g (+ 2 3)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids val body) #:transparent) ;let ([id val]... body))


;; Parser
(define (parse sexp) 
  (match sexp
    [(? integer?) ;is it an int
     (int-exp sexp)]
    [(list '+ lhs rhs) ;is it a string of the form (+ lhs rhs)
     (arith-exp "plus" (parse lhs) (parse rhs))]
     [(list '* lhs rhs) 
     (arith-exp "times" (parse lhs) (parse rhs))]
    [(? symbol?)(var-exp sexp)]
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]




    ))
#|
The string will be turned to a sexp using reader then the sexp is fed to the parser to become a decorated syntax tree


|#


#|-----------------------------------------------------------------------------
;; Interpreter

- The interpreter's job is the take the (decorated) syntax tree and evalute it!
-----------------------------------------------------------------------------|#

;; Interpreter, essential a pattern matching that decorted syntax trees
(define (eval expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; arithmetic
      [(int-exp val) val]
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]

      ;; variable binding
      #; [(var-exp id)
          (cdr (assoc id env))]

      ;; variable binding with error handling
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair (cdr pair) (error (format "~a not bound!" id))))]

      ;; let expression with a single variable
      #; [(let-exp (list (var-exp id)) (list val) body)
          (eval-env body (cons (cons id (eval-env val env)) env))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v) (eval-env v env)) val))])
         (eval-env body (append vars env)))]
      
      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))

    
#|
the let special form makes enviroment

|#


(define (repl)
  (let ([stx (parse (read))])
    (println (eval stx))
    (repl)))