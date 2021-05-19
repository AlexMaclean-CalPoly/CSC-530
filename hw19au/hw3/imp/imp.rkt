#lang rosette

(provide (all-defined-out))


; Lifts Racket dictionaries to work in Rosette (assuming the keys are always
; concrete, which is true in our case).
(require rosette/lib/lift)
(define-lift env-ref [(dict? symbol?) dict-ref])
(define-lift env-set [(dict? symbol? integer?) dict-set])

; Returns the result of evaluating the given IMP conditional C in the
; environment env. IMP conditional (i.e., boolean) expressions are built out of
; boolean constants true / false, comparisons of IMP integer expressions, and the
; application of logical connectives to conditional expressions.
(define (interpretC C env)
  (match C
    ['true #t]
    ['false #f]
    [`(= ,E1 ,E2)  (= (interpretE E1 env) (interpretE E2 env))]
    [`(<= ,E1 ,E2) (<= (interpretE E1 env) (interpretE E2 env))]
    [`(>= ,E1 ,E2) (>= (interpretE E1 env) (interpretE E2 env))]
    [`(< ,E1 ,E2)  (< (interpretE E1 env) (interpretE E2 env))]
    [`(> ,E1 ,E2)  (> (interpretE E1 env) (interpretE E2 env))]
    [`(! ,C1)      (! (interpretC C1 env))]
    [`(&& ,C1 ...) (apply && (map (curryr interpretC env) C1))]
    [`(|| ,C1 ...) (apply || (map (curryr interpretC env) C1))]
    [`(=> ,C1 ,C2) (=> (interpretC C1 env) (interpretC C2 env))]
    [`(<=> ,C1, C2)(<=> (interpretC C1 env) (interpretC C2 env))]))


; Returns the result of evaluating the given IMP integer expression in the
; environment env. IMP integer expressions are built out of integer constants,
; variables, and the application of arithmetic operators to integer expressions. 
(define (interpretE E env)
  (match E
    [(? integer?) E]
    [(? symbol?)  (env-ref env E)]
    [`(- ,E1 ..1) (apply - (map (curryr interpretE env) E1))]
    [`(+ ,E1 ...) (apply + (map (curryr interpretE env) E1))]
    [`(* ,E1 ...) (apply * (map (curryr interpretE env) E1))]))

; Gives semantics to new IMP statements when they are added to the language.
(define interpretS+
  (make-parameter
   (lambda (S env)
     (raise-argument-error 'interpretS+ "IMP statement" S))))

; Returns a new environment that is the result of evaluating the IMP statement
; stmt in the environment env, unless the execution aborts, in which case
; interpretS returns #f. IMP environments are modeled as (immutable) associative
; lists, which bind symbols representing variable names to values. We use env-ref
; and env-set to read / functionally update a given variable (key) in the
; enviornment. All variables bound in an IMP environment are of type integer, and
; this is enforced by the interpreter. Finally, this intepreter allows the IMP
; language to be extended with new statements, which are passed to the interpretS+
; procedure. By default, interpretS+ throws an error.
(define (interpretS S env)
  (and env  
   (match S
     [`(,(? keyword?) ,_ ...) env] ; proof annotations are ignored at runtime
     [`(skip) env]
     [`(abort) #f]
     [`(begin) env]  
     [`(begin ,S1 ,S2 ...)
      (interpretS `(begin ,@S2) (interpretS S1 env))]
     [`(:= ,x ,E)
      (env-set env x (interpretE E env))]
     [`(if ,C ,S1 ,S2)
      (let ([envif (if (interpretC C env) (interpretS S1 env) (interpretS S2 env))])
        (and envif (take envif (length env))))]
     [`(while ,C ,_ ...  ,B)  
      (interpretS `(if ,C (begin ,B ,S) (begin)) env)]
     [_ ((interpretS+) S env)])))

; Returns the result of evaluating the given IMP program on the provided inputs,
; or #f if the program aborts.
(define (interpret prog inputs)
  (match-define `(procedure ,_ (,args ...) : (,rets ...) ,_ ... ,S) prog)
  (define env (interpretS S (map cons args inputs)))
  (and env (for/list ([ret rets]) (env-ref env ret))))

; An imp object consists of an abstract syntax tree (AST) for an IMP 
; program, represented as a nested list of symbols (an s-expression), 
; as well as a Racket procedure that applies the IMP interpreter to this
; AST and given inputs.  Applying an imp object to inputs has the same
; effect as interpreting its AST on those inputs.
(struct imp (ast proc)
  #:transparent
  #:property prop:procedure
  [struct-field-index proc])

; Binds the identifier id to a program object that represents 
; the given IMP program.
(define-syntax (procedure stx)
  (syntax-case stx (:)
    [(_ id (arg ...) : (ret ...) contract ... stmt)
     (syntax/loc stx 
       (define id 
         (let* ([ast `(procedure id (arg ...) : (ret ...) contract ... stmt)]
                [id (lambda (arg ...) (interpret ast (list arg ...)))])
           (imp ast id))))]))




