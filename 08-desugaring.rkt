#lang racket


#|-----------------------------------------------------------------------------
;; Desugaring



e.g., support for lambda and function application with > 1 params/args

    (lambda (x y z ...)     can be written as     (lambda (x)
      body)                                         (lambda (y)
                                                      (lambda (z)
                                                        ...
                                                          body)))

  if we ensure that all function applications of the form (f x y z ...)
  are rewritten as ((((f x) y) z) ...)
-----------------------------------------------------------------------------|#

;; Some test cases
(define p1 '(lambda (x y z) (* x (+ y z))))

(define p2 '(f x y z))

(define p3 '((lambda (x y z) (* x (+ y z))) 2 3 4))


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
(struct app-exp (fn args) #:transparent)


;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; arithmetic expression
    [(list (and op (or '+ '*)) lhs rhs)
     (arith-exp (symbol->string op) (parse lhs) (parse rhs))]
    
    ;; identifiers (variables)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]
    
    ;; lambda expressions -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Desugar-er -- i.e., syntax transformer
(define (desugar exp)
  (match exp
    (_ exp)))


;; function value + closure
(struct fun-val (id body env) #:transparent)


;; Interpreter
(define (eval expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literals
      [(int-exp val) val]

      ;; arithmetic expressions
      [(arith-exp "+" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "*" lhs rhs)
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
       (fun-val id body env)] ; store current env in closure
      
      ;; function application (in lexical scope)
      [(app-exp f arg)
       (match-let ([(fun-val id body clenv) (eval-env f env)]
                   [arg-val (eval-env arg env)])
         (eval-env body (cons (cons id arg-val) clenv)))] ; eval in closure

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))


;; REPL
(define (repl)
  (let ([stx (desugar (parse (read)))]) ; added desugaring step
    (when stx
      (println (eval stx))
      (repl))))