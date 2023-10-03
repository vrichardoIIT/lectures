#lang racket

(require macro-debugger/stepper
         (for-syntax racket/base)
         (for-syntax racket/list))

#|------------------------------------------------------------------------------
;; Syntax objects

- syntax objects attach syntactical context to sexps
------------------------------------------------------------------------------|#

(define s1 (syntax "hello world")) ;contextual infomation on the string 'hello world'

(define s2 (syntax (foo bar 1 2 3))) ;a special form, if syntax is a function it would need to evaluate foo first

(define s3 #'(foo bar 1 2 3)) ; #' is shorthand for `syntax`

(define s4 #'(if (< 1 2) "true" "false"))

(define s5 #`(if #,(< 1 2) "true" "false")) ; "#`" acts as a quasiquote, and will evaluate (< 1 2 ) in this case 

 (values
    (syntax-source s3)
    (syntax-line s3) ; where the code is, here line 
    (syntax-column s3)
    (syntax->datum s3)) ;gives body of special form

; '(foo bar 1 2 3)) ->datum foo = 1st element of list, 2nd bar, 3rd 1, ....

;; `eval-syntax` evaluates syntax objects, like `eval` for sexps

#; (eval-syntax s5)




#|------------------------------------------------------------------------------
;; Syntax transformers, aka "Macros"

Takes a syntax object then manipulate it in some ways, returning a new syntax object and evalaute


------------------------------------------------------------------------------|#

;; `define-syntax` defines a macro

(define-syntax say-hi
  (lambda (stx) ; `stx` refers to the original syntax object 
    #`(quote (hi #,stx)))) 


(define-syntax (say-hi2 stx) ; alternate form
  #`(quote (hi #,stx)))

;prefix => (+ x y)
;; define a macro that supports infix notation (for binary functions)
(define-syntax (infix stx)
  (let ([infix-exp (second (syntax->datum stx))])
    #`(#,(second infix-exp) #,(first infix-exp) #,(third infix-exp)))) 

#; (syntax->datum (expand-once #'(infix (2 * 3))))


;; define our own `if` special form (based on cond)
(define-syntax (my-if stx)
  (let ([datum (syntax->datum stx)])
    #`(cond [#,(second datum) #,(third datum)]
            [else #, (fourth datum)])))

;(my-if (< 1 2) (println "yes") (println "no"))

#; (syntax->datum
    (expand-once #'(my-if (< 1 2)
                          (println "yes")
                          (println "no"))))

#; (syntax->datum
    (expand #'(my-if (< 1 2)
                     (println "yes")
                     (println "no"))))


;; `syntax-case` lets us pattern match on syntax objects
(define-syntax (infix2 stx)
  (syntax-case stx ()
    [(_ (lhs op rhs))
     ;(infix (1 + 2)) here inflix = _, 1hs = 1, ops = +, and rhs = 2, 
     #'(op lhs rhs)])) ;then change and evalute in the form (+ lhs rhs)


(define-syntax (my-if-2 stx)
  (syntax-case stx (then otherwise) ; parentheses enclose syntax "literals"
    [(_ test exp1 exp2) ; matched ids can be used directly in syntax forms
     #'(cond [test exp1]
             [else exp2])]
    [(_ test then exp1 otherwise exp2)
     #'(cond [test exp1]
             [else exp2])]))

#; (syntax->datum
    (expand-once #'(my-if-2 (< 1 2)
                            (println "yes")
                            (println "no"))))

#; (syntax->datum
    (expand-once #'(my-if-2 (< 1 2)
                            then (println "yes")
                            otherwise (println "no"))))


;; define-syntax-rule is shorthand for a limited form of `syntax-case`
(define-syntax-rule (my-if-3 test exp1 exp2)
  (cond [test exp1]
        [else exp2]))


(define-syntax-rule (swap! x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))


;; define a macro that implements a loop
(define-syntax-rule (loop n body)
  (let rec ([i 0])
    (when (< i n)
      body
      (rec (add1 i)))))

;; can we interact with the `i` introduced in the `loop` macro from "outside"?
#; (loop 10 (println i)) ;error 
#; (let ([i 10]) (loop i (println i))) ;will print 10, 10times the i that is implemented in the code is not the same with the i in loop

;; define a macro that implements a "for" loop, exposing the iterator var
(define-syntax-rule (for-loop var n body)
  (let rec ([var 0])
    (when (< var n)
      body
      (rec (add1 var)))))
;


#|------------------------------------------------------------------------------
;; Hygiene

- Racket macros are "hygienic" by design -- i.e., identifiers introduced by
  a macro exist in a separate lexical context from where it is called, and
  so cannot be accidentally (or intentionally) used by call-site code.
------------------------------------------------------------------------------|#

;; syntax objects created with `syntax` are hygienic -- their bindings are
;; determined by their lexical context (i.e., where they are defined):
(define x 440)
(define-syntax (hygienic stx)
  #'(println x))

#; (values
    (hygienic)
    (let ([x 10]) (hygienic))) ;will use 440 and fail to find the new x

(define-syntax (hygienic2 stx)
  #'(define foo 440))

;foo is undefine

;; but `datum->syntax` allows us to "break" hygiene by inheriting the lexical
;; context of some other syntax object (e.g., from the call site)
(define-syntax (unhygienic stx)
  (datum->syntax stx '(println x)))

#; (values
    (unhygienic)
    (let ([x 10]) (unhygienic)))

(define-syntax (unhygienic2 stx)
  (datum->syntax stx '(define bar 440)))

#; (begin (unhygienic2)
       (println bar))


;; "Anaphoric if": a convenient programmer-defined control structure
;; 
;; an·a·phor | ˈanəˌfôr | (Noun)
;; - a word or phrase that refers to an earlier word or phrase
;;   (e.g., in "my cousin said she was coming", "she" is used as an
;;   anaphor for my cousin).

#; (aif (compute-test-result ...)  ; may be a lengthy computation
        (use it)      ; `it` refers to the result of the computation
        (else-case))  


;; what's wrong with the following attempt?
(define-syntax-rule (aif test exp1 exp2) ;hygenic, 'it' is undefine at call site
  (let ([it test])
    (if it
        exp1
        exp2)))


;; can you "fix" this? (need to manually break hygiene)
(define-syntax (aif-2 stx)
  (let ([sexp (syntax->datum stx)])
    (datum->syntax stx
                   `(let ([it ,(second sexp)])
                      (if it
                          ,(third sexp)
                          ,(fourth sexp))))))


;; still not perfect!