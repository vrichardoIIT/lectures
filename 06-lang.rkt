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
(module
    foo ;module name
  racket ;evaluator/ initial import

  (provide bar)

  (define (bar)
    (println "hello bar"))

  (define (bas)
    (println "hello bas"))
  )

(require 'foo)


#|-----------------------------------------------------------------------------
;; The Reader and Expander

So what *really* happens when we load a source file into a Racket interpreter?

- The `#lang` line (which we previously always wrote as `#lang racket`) tells
  Racket where to find the language's **reader**.

- The reader's job is to read in code from the source file and convert it into
  syntax objects

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

;; Implement a reader & expander for the "program" in 06-lang-demo.rkt

#;(port->line (open-input-file "06-lang-demo.rkt"))

(provide read-syntax)
(define (read-syntax path port)
  (let* ([src-lines (filter non-empty-string?
                            (map string-trim (port->lines port)))])
    (datum->syntax #f
                   `(module demo racket
                      @src-lines))))

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
