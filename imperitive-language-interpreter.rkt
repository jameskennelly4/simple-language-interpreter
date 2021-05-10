#lang racket

; OO Interpreter Project Programming Language Concepts 2021
; Authors: Barry McCoy, James Kennelly, Paul Rodriguez

(require "classParser.rkt")

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file classname)
    (scheme->language
     (call/cc
      (lambda (return)
        (create-class-closures (parser file) classname (newenvironment) return (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; Higher level function to go through and create class closures for all classes
(define create-class-closures
  (lambda (class-statement-list classname class-closures return throw)
    (if (null? class-statement-list)
        (run-main classname class-closures)
        (create-class-closures (cdr class-statement-list) classname (insert classname (create-class-closure (car class-statement-list)) class-closures) return throw))))

; Function to create a class closure containing the parent class and instance fields and methods
(define create-class-closure
  (lambda (class)
    (cons (cons (parent-class class) '()) (instance-fields-and-methods (cadddr class)))))

; Helper function to get parent class
(define parent-class
  (lambda (class)
    (if (parent-class-exists? class)
        (extends-class class)
        (cadr class))))

; Helper function for parent class
(define extends-class
  (lambda (class)
    ((car (cdaddr class)))))

; Function to check if there is a parent class
(define parent-class-exists?
  (lambda (class)
    (not (null? (caddr class)))))

; Helper function to get all instance fields and methods for the class closures
(define instance-fields-and-methods
  (lambda (class-body)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-class-fields-and-methods-statement-list class-body (newenvironment) return (lambda (v env) (myerror "Uncaught exception thrown")))))))) 

; Returns the environment created after interpreting all of the class fields and methods
(define interpret-class-fields-and-methods-statement-list
  (lambda (statement-list environment return throw)
    (if (null? statement-list)
        (return environment)
        (interpret-class-fields-and-methods-statement-list (cdr statement-list) (interpret-global-statement-list (car statement-list) environment throw) return throw))))   

; Example test 1: var a = new A();
; should store "a" as the instance with a closure of the run time type "A" and the fields/methods of "a"
(define create-instance-closure
  (lambda (class-name global-state)
    (cons (cons class-name '()) (cons (lookup class-name (get-class-closures class-name)) '()))))

(define get-class-closures
  (lambda (global-state)
    (cdr global-state)))

; Creates an outer layer that just does M-state functions for variable declarations and function definitions, and then runs the main function
(define create-global-state
  (lambda (statement-list global-state return throw)
    (if (null? statement-list)
        (return (run-main global-state)) 
    (create-global-state (cdr statement-list) (interpret-global-statement-list (car statement-list) global-state throw) return throw))))

; Interprets the variable declarations and function definitions of the outer layer
(define interpret-global-statement-list
  (lambda (statement global-state throw)
    (cond
      ((eq? 'var (statement-type statement)) (interpret-declare statement global-state throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement global-state))
      ((eq? 'static-function (statement-type statement)) (interpret-static-function statement global-state))
      (else (myerror "Unknown global statement:" (statement-type global-state))))))

; looks up and executes the main function of the given class
(define run-main
  (lambda (classname global-state)
    (interpret-main (cadr (lookup 'main (cdr (lookup 'A global-state)))) global-state)))

; Takes in the body of the main function and the global state, and then evaluates the body
(define interpret-main
  (lambda (main-function-body global-state) 
    (call/cc
      (lambda (return)
        (pop-frame (interpret-statement-list main-function-body (push-frame global-state) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; Interprets a list of statements.  The environment from each statement is used for the next ones
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; Interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return break continue throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall statement environment throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return break continue throw)
    (return (eval-expression (get-expr statement) environment throw))))

; Function to interpret a declare statement and handle if it is an object
(define interpret-declare
  (lambda (statement environment throw)
    (if (exists-declare-value? statement)
        (if (list? (caddr statement))
            (insert (get-declare-var statement) (lookup (car (cdaddr statement)) environment) environment)
            (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment))
     (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw)) 
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
; Mstate({ <body> }, state) = pop-frame (Mstate (<body>, pushframe(state)))
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Adds a new binding to the environment. The function name is bound to its closure
(define interpret-function
  (lambda (statement environment)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (create-closure (operand2 statement) (operand3 statement) environment) environment)
        (insert (get-declare-var statement) 'novalue environment))))

(define interpret-static-function
  (lambda (statement environment)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (create-closure (operand2 statement) (operand3 statement) environment) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Create closure helper method
(define create-closure
  (lambda (formal-params body environment)
    (cons formal-params (cons body (cons environment '())))))

; Takes in a function call and sends to eval-function to evaluate it 
(define interpret-funcall
  (lambda (statement environment throw)
    (if (list? (cadr statement))
        (eval-function (caddr (cadr statement)) (get-actual-params statement) (cons (cadr (lookup (cadr (cadr statement)) environment)) '()) throw)
        (eval-function (cadr statement) (get-actual-params statement) environment throw))))

; Evaluates a function call. It does this by:
; (a) Creating a function environment using the closure function on the current environment
; (b) Evaluates each actual parameter in the current environment and binds it to the formal parameter in the function environment
; (c) Interprets the body of the function with the function environment
(define eval-function
  (lambda (name actual-params environment throw)
    (define closure (lookup name environment))
    (define fstate1 (caddr closure))
    (define formal-params (car closure))
    (define fstate2 (create-bindings formal-params actual-params environment (push-frame fstate1) throw))
    (define body (cadr closure))
    (pop-frame (interpret-statement-list body (add-frame (car fstate2) environment) (lambda (v) v) (lambda (s) (myerror "Break used outside of loop")) (lambda (t) (myerror "Continue used outside of loop")) throw))))

; Creates the bindings between the formal parameters and actual parameters in the environment
(define create-bindings
  (lambda (formal-params actual-params state environment throw)
    (if (eq? (length formal-params) (length actual-params))
        (if (null? formal-params)
            environment
            (create-bindings (cdr formal-params) (cdr actual-params) state (insert (car formal-params) (eval-expression (car actual-params) state throw) environment) throw))
        (myerror "Mismatched parameters"))))
    
; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      ((eq? 'funcall (operator expr)) (interpret-funcall expr environment throw))
      ((eq? 'dot (operator expr)) (eval-dot expr environment throw))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Evaluates all cases of the dot operator by checking which environment we should be looking for the value in and then looking in that respective environment
(define eval-dot
  (lambda (expr environment throw)
    (cond
      ((eq? (cadr expr) 'this) (lookup (operand2 expr) (get-class-closures environment)))
      ((eq? (cadr expr) 'super) (eval-binary-op2 expr (lookup (car (lookup (operand1 expr) environment)) (cadr (lookup operand1 environment))) environment throw) environment throw)
      ((function-exists (operand2 expr) (lookup (operand1 expr) environment)) (eval-function (operand2 expr) (get-actual-params (lookup (operand1 expr) environment)) (lookup (operand2 expr) (cons (cadr (lookup (operand1 expr))) '())) throw))
      (else (eval-expression (lookup (operand2 expr) (cons (cadr (lookup (operand1 expr) environment)) '())) environment throw)))))


(define function-exists
  (lambda (func-name instance)
    (exists? func-name (cadr instance))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

(define contains?
  (lambda (list x)
    (cond
      ((null? list) #f)
      ((not (list? list)) #f)
      ((equal? (car list) x) #t)
      (else (contains? (cdr list) x)))))

(define get-actual-params
  (lambda (statement)
    (cddr statement)))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; add environment frame onto the top of the global state
(define add-frame
  (lambda (environment global-state)
    (cons environment global-state)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (if (not (pair? environment))
        environment
        (cdr environment))))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))


; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

