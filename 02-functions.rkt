#lang racket

(require 2htdp/image) ; for a bit of fun with pictures

#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Some more special forms

- `if`: if-then-else
- `when`: if-then
- `cond`: multi-way conditional
-----------------------------------------------------------------------------|#

#; (if bool-expr then-expr else-expr)

#; (when bool-expr then-expr ...)

#; (cond [bool-expr1 result1]
         [bool-expr2 result2]
         [bool-expr3 result3]
         [else result4])


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
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Even more special forms

- `apply`: apply a function to a list of arguments
- `compose`: returns a function that composes the given functions
- `curry`: returns a version of a function that can be partially applied
- `eval`: evaluate a sexp
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used
-----------------------------------------------------------------------------|#
