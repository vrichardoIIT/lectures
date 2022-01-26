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
-----------------------------------------------------------------------------|#

#; (begin body ...)

#; (if test-expr then-expr else-expr)

#; (when test-expr body ...)

#; (cond [expr1 body ...]
         [expr2 body ...]
         [else body ...])


#|-----------------------------------------------------------------------------
;; Equality tests

- `=` for numbers
- `eq?` for pointer-based equality
- `equal?` for value-based equality
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Recursion
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.

Some useful built-in HOFs and related functions:
- `eval`: evaluates a sexp
- `apply`: apply a function to a list of arguments
- `curry`: returns a version of a function that can be partially applied
- `compose`: returns a function that is the composition of two other functions
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used
-----------------------------------------------------------------------------|#
