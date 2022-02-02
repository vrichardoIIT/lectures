#lang racket

(require racket/trace) ; for dynamic function call traces
(require 2htdp/image) ; for a bit of fun with pictures


#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions
-----------------------------------------------------------------------------|#


#|-----------------------------------------------------------------------------
;; Some more special forms

- `begin`: sequences multiple sexps; evaluates to the result of the last
- `if`: if-then-else
- `when`: if-then
- `cond`: multi-way conditional
- `match`: pattern match
- `set!': variable assignment
-----------------------------------------------------------------------------|#

#; (begin body ...)

#; (if test-expr then-expr else-expr)

#; (when test-expr body ...)

#; (cond [expr1 body ...]
         [expr2 body ...]
         [else body ...])

#; (match val
     [pattern1 body ...]
     [pattern2 body ...]
     [pattern3 body ...])

#; (set! id expr)


#|-----------------------------------------------------------------------------
;; Equality tests

- `=` for numbers
- `eq?` for pointer-based equality
- `equal?` for value-based equality
-----------------------------------------------------------------------------|#

#; (values 
     (= 2 2)
     (= 2 2.0)
     (= 2 2.01)
     (= 2 2.0000000000000000001)
     (= 2 8/4)
     (eq? 'a 'a)
     (eq? "hello world" "hello world")
     (eq? '(a b c) '(a b c))
     (let ([lst '(a b c)]) (eq? lst lst))
     (equal? "hello world" "hello world")
     (equal? '(a b c) '(a b c)))


#|-----------------------------------------------------------------------------
;; Recursion
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#