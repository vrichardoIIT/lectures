#lang racket

#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions
-----------------------------------------------------------------------------|#

(define f1 (lambda (x) (+ x 1))) ;(functionname (lambda var body))
;(f1 var): (f1 4) = 5
(define f2 (lambda (x y z)
             (println x)
             (println y)
             (println z)
             (* x (+ y z))))

(define (f3 x) (add1 x)) ;(functionname var)

(define (f4 x y z)
  (println x)
  (println y)
  (println z)
  (* x (+ y z)))


;; `values` and `let-values` can be used to return/retrieve multiple values
;; from/to a function call.
(define (quad-roots a b c)
  (let* ([disc (- (* b b) (* 4 a c))]
         [sqr-disc (sqrt disc)])
    (values (/ (+ (- b) sqr-disc) (* 2 a))
            (/ (- (- b) sqr-disc) (* 2 a)))))

(define (test-quad-roots r1 r2)
  (let ([a 1]
        [b (- (+ r1 r2))]
        [c (* r1 r2)]) ; based on (x + r1)(x + r2)
    (let-values ([(rr1 rr2) (quad-roots a b c)])
      (println rr1)
      (println rr2))))


;; "rest" arguments
(define (f5 x y . z) ; `z` is a list of any & all args after `x` and `y`
  (println x)
  (println y)
  (println z))

(define (f6 . rest)
  (length rest))


;; lambda expressions also support rest arguments
(define f7
  (lambda args ; `args` is a list of all the arguments
    (length args)))

(define f8
  (lambda (x . rest)
    (println x)
    (println rest)))


#|-----------------------------------------------------------------------------
;; Some more special forms

- `if`: if-then-else
- `begin`: sequences multiple sexps; evaluates to the result of the last
- `when`: if-then
- `cond`: multi-way conditional
- `case`: dispatch
- `match`: pattern match
- `set!`: mutation!
-----------------------------------------------------------------------------|#

(define (say-hi-1 name)
  (if (equal? name "Jane")
      (println "Me Tarzan!")
      (printf "Hello, ~a~n" name))) ; `printf` does interpolation/formatting


;; digression: equality testing
(list
 (list
  ;; `=` for numbers
  (= 2 2)
  (= 2 2.0)
  (= 2 2.01)
  (= 2 2.0000000000000000001)
  (= 2 8/4))

 (list
  ;; `eq?` for pointer comparison
  (eq? 'a 'a)
  (eq? "hello world" "hello world")
  (eq? '(a b c) '(a b c))
  (let ([lst '(a b c)])
    (eq? lst lst)))

 (list
  ;; `equal?` for value comparison
  (equal? "hello world" "hello world")
  (equal? '(a b c) '(a b c))))
 

;; use `string-ref` to get a char by index in a string
(define (say-hi-2 name)
  (if (equal? (string-ref name 0) #\J)
      ;; `begin` for multi-expression "blocks"
      (begin (println "Me Tarzan!")
             (printf "You ~a!~n" name))
      (printf "Hello, ~a~n" name)))


;; `when` is an `else`-less `if`, with an automatic `begin`
(define (say-hi-3 name)
  (when (regexp-match? #rx"^Jan.*" name) ; regular exp matching!
    (println "Me Tarzan!")
    (printf "You ~a!~n" name)))


(define (say-hi-4 name)
  ;; `cond` is a multi-way branch
  (cond [(equal? name "Jane")
         (println "Me Tarzan!")]
        [(regexp-match? #rx"^Jan.*" name)
         (printf "You ~a?~n" name)]
        [else
         (printf "Hello, ~a~n" name)]))


(define (say-hi-5 name)
  ;; `case` checks a value against multiple options using `equal?`
  (case (char-downcase (string-ref name 0))
    [(#\j) (println "Me Tarzan!")]
    [(#\a #\e #\i #\o #\u) (println "Ewoh!")]
    [else (printf "Hello, ~a~n" name)]))


(define (say-hi-6 name)
  ;; `match` is a general purpose pattern matcher with its own special language
  (match name
    ["Jane" (println "Me Tarzan!")]
    [(regexp #rx"^Jan.*") (println "Me Tarzan?")]
    [(? string?) (printf "Hello, ~a~n" name)]
    [(list x y) (printf "Hello, ~a and ~a~n" x y)]
    [(list "Jane") (println "Hello, Jane-in-a-list")]
    [(list "Jane" x "Jane") (printf "Hello, ~a-between-Janes~n" x)]
    [(list x y x) (printf "Hello, ~a-between-~as~n" y x)]
    [(list _ _ _ "Jane") (println "Hi, Jane in 4th place")]
    [(list x ...) (printf "Hi, lots of ~as~n" x)]
    [(or 'Jane '(Jane)) (println "Hi, Jane in hiding")]
    [_ (println "Stranger, Danger!")]))


(define (say-hi-7 name)
  (let ([greetings '("Hello" "Hola" "你好")]
        [index 0])
    ;; what are we returning? what is going on?!
    (lambda ()
      (printf "~a, ~a~n" (list-ref greetings index) name)
      (set! index (remainder (add1 index) (length greetings))))))
