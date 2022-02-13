#lang racket

#|-----------------------------------------------------------------------------
;; Adding functions & closures to our interpreter
-----------------------------------------------------------------------------|#

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
    
    ;; lambda expressions
    [(list 'lambda (list id) body)
     (lambda-exp id (parse body))]

    ;; function application
    [(list f arg)
     (app-exp (parse f) (parse arg))]

    ;; basic error handling
    [_ #f #; (error (format "Can't parse: ~a" sexp))]))


#|-----------------------------------------------------------------------------
;; Functions
-----------------------------------------------------------------------------|#

;; function value
#; (struct fun-val (id body) #:transparent)


;; Interpreter
#; (define (eval expr)
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
          (fun-val id body)]
      
         ;; function application
         [(app-exp f arg)
          (match-let ([(fun-val id body) (eval-env f env)]
                      [arg-val (eval-env arg env)])
            (eval-env body (cons (cons id arg-val) env)))]

         ;; basic error handling
         [_ (error (format "Can't evaluate: ~a" expr))])))


;; REPL
(define (repl)
  (let ([stx (parse (read))])
    (when stx
      (println (eval stx))
      (repl))))


;; Some test cases
(define p1 '(lambda (x) (+ x 1)))

(define p2 '((lambda (x) (+ x 1)) 10))

(define p3 '(let ([x 10])
              (lambda (y) (+ x y))))

(define p4 '(let ([x 10])
              ((lambda (y) (+ x y)) 20)))

(define p5 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (let ([x 20])
                (f x))))


#|-----------------------------------------------------------------------------
;; Closures
-----------------------------------------------------------------------------|#

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
       (fun-val id body env)]
      
      ;; function application (in lexical scope)
      [(app-exp f arg)
       (match-let ([(fun-val id body clenv) (eval-env f env)]
                   [arg-val (eval-env arg env)])
         (eval-env body (cons (cons id arg-val) clenv)))]

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))