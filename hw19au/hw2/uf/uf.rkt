#lang rosette

(provide (all-defined-out))

; This module should provide UF-based implementations of
; all procedures that need to be abstracted in order to
; improve the performance of the queries in matrix.rkt.
; If you want to replace a Rosette procedure, such as -,
; with a UF, all you need to do is re-define the procedure
; - in this file. Doing so causes every call to - in
; a module that imports uf.rkt to refer to your custom -
; definition. For example, uncommenting the following definition
; would cause every call to - in matrix.rkt to be refer to this -.
;
; (define (- a b)
;   (define-symbolic uf- (~> real? real? real?)) 
;   (uf- a b))
;
; So evaluating (- 1 2) would return the symbolic value (app uf- 1 2),
; which represents the application of the uninterpreted function uf-
; to the inputs 1, 2. 
;
; Note that many Rosette procedures take a variable number of arguments.
; Your re-defined procedure should provid the same interface, or the code
; that calls it may crash. For example, Rosette's - takes as input 1 or
; more arguments, so the above definition would only work for clients that
; call - with 2 arguments. You might want to read the docs to find out the
; arity of the procedure proc that you want to abstract with a UF.
;
; See https://docs.racket-lang.org/rosette-guide/sec_UF.html for more
; details on Rosette's support for UFs.

