#lang racket

(require racket/trace)

#|-----------------------------------------------------------------------------
;; Adding Functions & Closures

We will add functions (as lambda expressions) and function applications to our
language. Our functions will have exactly one formal parameter each.

E.g.,

- lambda definition: `(lambda (x) (+ x 1))`

- function application: `((lambda (x) (+ x 1)) 10)`

Though our language will not support named functions a la Racket's `define`,
we can use `let` to bind identifiers to lambdas. E.g.,

  (let ([f (lambda (x) (+ x 1))])
    (f 10))
-----------------------------------------------------------------------------|#

;; Some test cases
(define p1 '(lambda (x) (+ x 1)))

(define p2 '((lambda (x) (+ x 1)) 10))

(define p3 '(let ([f (lambda (x) (+ x 1))])
              (f 10)))

;; p4-p5 for testing strict/lazy eval
(define p4 '(let ([x (+ 1 2)])
              20))

(define p5 '(let ([f (lambda (x) 10)])
              (f (+ 1 2))))

;; p6-p9 for testing closures
(define p6 '(let ([x 10])
              (lambda (y) (+ x y))))

(define p7 '(let ([x 10])
              ((lambda (y) (+ x y)) 20)))

(define p8 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (let ([x 20])
                (f x))))

(define p9 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (f 20)))


;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)


;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; arithmetic expression
    [(list '+ lhs rhs)
     (arith-exp "PLUS" (parse lhs) (parse rhs))] 
    [(list '* lhs rhs)
     (arith-exp "TIMES" (parse lhs) (parse rhs))]
    
    ;; identifiers (variables)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]
    
    ;; lambda expressions
    [(list 'lambda (list id) body)
     (lambda-exp id (parse body))]

    ;; function application
    [(list f arg)
     (app-exp (parse f) (parse arg))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Interpreter (functions with dynamic scoping & strict evaluation)
(define (eval-dyn-strict expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]
         
      ;; arithmetic expressions
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         

      ;; variable binding (LET expression)
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair
             (cdr pair)
             (error (format "~a not bound!" id))))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v) ; evaluate values at bind-time
                               (eval-env v env)) ;evaluate let values
                             val))])
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body) ;return expression, this is like a special form
       (lambda-exp id body)] ; why don't we evaluate the body?
      
      ;; function application (in dynamic scope)
      [(app-exp f arg)
       (match-let ([(lambda-exp id body) (eval-env f env)] ;evaluate the function first, make sure it matches the lambda exp
                   [arg-val (eval-env arg env)]) ; call-by-value, eval argument and set to arg-val
         (eval-env body (cons (cons id arg-val) env)))] 

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))


;; Interpreter (functions with dynamic scoping & lazy evaluation)
(define (eval-dyn-lazy expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]
         
      ;; arithmetic expressions
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         

      ;; variable binding (LET)
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair
             ;; evaluate when derefenced (not very efficient!)
             (eval-env (cdr pair) env) ; eval when binding variable
             (error (format "~a not bound!" id))))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id val)]) ; no value evaluation!
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body)
       (lambda-exp id body)]
      
      ;; function application (in dynamic scope)
      [(app-exp f arg)
       (match-let ([(lambda-exp id body) (eval-env f env)])
         (eval-env body (cons (cons id arg) env)))] ; call-by-name

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))


;; REPL
(define (repl)
  (let ([stx (parse (read))])
    (when stx
      (println (eval stx))
      (repl))))


#|-----------------------------------------------------------------------------
;; Closures

Idea: free identifiers in a function are bound *at the time of definition*.
This is called "lexical scoping". It requires that we attach a copy of the
environment (including all relevant bindings) to the function value at the time
the creating lambda expression is evaluated.

A function therefore "closes over" bindings in its environment. We call such a
function a "closure".
-----------------------------------------------------------------------------|#

;; function value + closure
(struct fun-val (id body env) #:transparent)


;; Interpreter (functions with lexical scoping / closures & strict evaluation)
(define (eval expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]

      ;; arithmetic expressions
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         
      
      ;; variable binding
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair (cdr pair) (error (format "~a not bound!" id))))]

      ;; let expression with multiple variables
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v) (eval-env v env)) val))])
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body)
       ;when we evaluate the lambda, save the current environment in th closure environment
       (fun-val id body env)] ; store current env in closure
      
      ;; function application (in lexical scope)
      [(app-exp f arg)
       (match-let ([(fun-val id body clenv) (eval-env f env)]
                   [arg-val (eval-env arg env)])
         (eval-env body (cons (cons id arg-val) clenv)))] ; eval in closure

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))