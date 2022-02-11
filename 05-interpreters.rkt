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
;; - `widget`
;; - `widget?`
;; - `widget-name`: retrieve `name` attribute
;; - `widget-purpose`: retrieve `purpose` attribute
;; - `widget-price`: retrieve `price` attribute

(define w1 (widget "wrench" "wrenching" 9.99))
(define w2 (widget "plier" "pliering" 12.99))


;; define a `doohickey` type that is a sub-type of `widget`
(struct doohickey widget (special-power) #:transparent)

(define d1 (doohickey "thingamajig" "thinging" 199.99 "time travel"))


#|-----------------------------------------------------------------------------
;; Scanner

Review: What is scanning?

- Raw text input => "tokens" / words
-----------------------------------------------------------------------------|#

#; (read)

;; `read` does much more than just scan our input! Because of how simple
;; Racket syntax is, `read` returns an unadorned, list-based "syntax tree"
;; based on the input -- i.e., an s-expression.


#|-----------------------------------------------------------------------------
;; Parser

Review: What is parsing?

- Tokens => Syntax tree

- Syntax tree = a representation of the syntactic structure of the code

  - We can "decorate" nodes of the tree with attributes useful for evaluation,
    optimization, etc.

Because Racket's `read` does so much for us, we already have a syntax tree!
We just need to decorate it now :)
-----------------------------------------------------------------------------|#

;;; Define some types for decorating our syntax tree

;; base expression type
(struct exp () #:transparent)

;; for arithmetic expressions
(struct int-exp exp (val) #:transparent)
(struct arith-exp exp (op lhs rhs) #:transparent)

;; for variables
(struct let-exp exp (ids vals body) #:transparent)
(struct var-exp exp (id) #:transparent)

  
;; Parser v1
#; (define (parse sexp)
     (cond
       [(number? sexp) (int-exp sexp)]
       [(equal? (first sexp) '+)
        (let ([lhs (second sexp)]
              [rhs (third sexp)])
          (void))]
       [(equal? (first sexp) '*)
        (let ([lhs (second sexp)]
              [rhs (third sexp)])
          (void))]))


;; Digression: pattern matching with `match`
(define (matcher sexp)
  (match sexp
    [(? number?) sexp]
    ['a 'apple]
    [(list _ _) 'any-two]
    [(list 'a _ _) 'a-and-two-things]
    [(list 'a 'b c d) (+ c d)]
    [(list a _ _ a) a]
    [(list 'foo (list b ...) c) (list b c)]
    [(widget "wrench" pur pri) pri]
    [(doohickey name _ _ _) name]
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Parser v2: using `match`
(define (parse sexp)
  (match sexp
    ;; for interpreter v1: arithmetic
    [(? number?)
     (int-exp sexp)]
    [(list '+ lhs rhs) (arith-exp "+" (parse lhs) (parse rhs))]
    [(list '* lhs rhs) (arith-exp "*" (parse lhs) (parse rhs))]
    #; [(list (and op (or '+ '*)) lhs rhs) ; alternative to above two patterns
        (arith-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; for interpreter v2: variables
    [(? symbol?)
     (var-exp sexp)]
    
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]

    ;; for interpreter v3: functions
    [(list 'lambda (list vars ...) body)
     `(,vars ,body)]
    
    [(list f args ...)
     `(,f ,@args)]

    ;; basic error handling
    [_ #f #; (error (format "Can't parse: ~a" sexp))]))


#|-----------------------------------------------------------------------------
;; Interpreter

Syntax tree => Evaluation
-----------------------------------------------------------------------------|#

;; Interpreter v1: Integers and Arithmetic
(define (eval expr)
  (match expr
    [(int-exp val) val]
    [(arith-exp "+" lhs rhs)
     (+ (eval lhs) (eval rhs))]
    [(arith-exp "*" lhs rhs)
     (* (eval lhs) (eval rhs))]

    ;; basic error handling
    [(not (? exp?)) (error "Encountered non-expression!")]
    [_ (error (format "Can't evaluate: ~a" expr))]))


;; Let's define a REPL!
(define (repl)
  (let ([stx (parse (read))])
    (when stx
      (println (eval stx))
      (repl))))


;; Interpreter v2: Adding variables
#; (define (eval expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      [(int-exp val) val]
      [(arith-exp "+" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "*" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]

      ;; variable binding
      [(var-exp id)
          (cdr (assoc id env))]

      ;; variable binding with error handling
      #; [(var-exp id)
          (let ([pair (assoc id env)])
            (if pair (cdr pair) (error (format "~a not bound!" id))))]

      ;; let expression with a single variable
      [(let-exp (list (var-exp id)) (list val) body)
       (eval-env body (cons (cons id (eval-env val env)) env))]

      ;; let expression with multiple variables
      #; [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v) (eval-env v env)) val))])
         (eval-env body (append vars env)))]

      ;; basic error handling
      [(not (? exp?)) (error "Encountered non-expression!")]
      [_ (error (format "Can't evaluate: ~a" expr))])))

