#lang racket

(require (for-syntax racket/base racket/list)
         racket/string)


#|-----------------------------------------------------------------------------
;; Modules

A Racket source file will typically evaluate to a single top-level `module`,
which encapsulates the definitions found in the file. The `#lang` line we've
been using is actually shorthand for a `module` declaration.
-----------------------------------------------------------------------------|#

;; E.g., a standalone module
#;(module
    foo ;module name
  racket ;evaluator/ initial import

  (provide bar) ;export function using the 'provide' special form, 

  (define (bar)
    (println "hello bar")) ;definition/function

  (define (bas)
    (println "hello bas"))
  )

#;(require 'foo);if we want to use this module when need to use 'require'


#|-----------------------------------------------------------------------------
;; The Reader and Expander

So what *really* happens when we load a source file into a Racket interpreter?

- The `#lang` line (which we previously always wrote as `#lang racket`) tells
  Racket where to find the language's **reader**.

- The reader's job is to read in code from the source file and convert it into
  syntax objects (scanner, a bit of a parcer)

- The top-level syntax object returned by the reader is a `module` which:

  - Contains all the sub-expressions read from the source file

  - Identifies the **expander** to which the forms will be passed

  - E.g., a file named "foo.rkt" containing:

       #lang racket
       exp ...

    becomes:

       (module foo racket
        exp ...)

- The expander's job is to figure out how to take the syntax objects produced
  by the reader and "expanding" them into forms that can be evaluated in Racket

  - Macros may be useful at this stage!

- The Reader and Expander give us everything we need to create new languages!
-----------------------------------------------------------------------------|#

;; Implement a reader & expander for the "program" in 06-lang-demo.rkt (basic I/O)

#;(open-input-file "06-lang-demo.rkt") ;open file - return input port
#; (port->lines (open-input-file "06-lang-demo.rkt")) ;reads content of file in a string

;file to port automatically
(provide read-syntax) ;make file a reader
(define (read-syntax path port) ;path to file and port to file, return syntac object

  (define (non-comment? line) ;take line and return true if not a comment
    (and (non-empty-string? line) ;not an empty string 
         (not (string-prefix? line "--")))) ;doesn't start with the "--" prefix

  (define (make-form line)
    (let ([lst (string-split line)])
      `(update ,(first lst) ,(string->number (second lst))))) ;use update, racket expender will not let, change expander to "06-lang.rkt" which will make it a reader and scaner
  
  (let* ([src-lines (filter non-comment? ;filter non empty strings, we can change non-empty-string? with non-comment?
                            (map string-trim (port->lines port)))]) ;convert port to line, trim empty lines at the end
    (datum->syntax #f ;return corrisponding systax object
                   `(module demo ;name
                      "06-lang.rkt" ;expander
                      ,@(map make-form src-lines)
                      players)))) ;,@ is like the python unpacking  ,@src-lines

(define players (make-hash))


(define (update name val)
  (hash-set! players name (+ val (hash-ref players name 0))))


(provide update players)


#|
(make-hash) creates hashtables (mutable)
(hash-set! hashtable key value) change mutable hashtable


(hash) immutable

(hash-update hashtable key function val)


(hash-ref hashtable key val) get value
|#




#|-----------------------------------------------------------------------------
;; Interposition points

Additional macros give us "hooks" into different stages of the process of
reading, expanding, and evaluating programs in our new language. They include:

- #%module-begin: module wrapper
- #%top-interaction: REPL wrapper
- #%datum: datum wrapper
- #%app: function application wrapper
- #%top: top-level identifier wrapper

Since we didn't use them in this (simple) language, we simply export the
default versions provided by racket/base.
-----------------------------------------------------------------------------|#

;; export default interposition macros
(provide #%module-begin
         #%top-interaction
         #%datum
         #%app
         #%top)
