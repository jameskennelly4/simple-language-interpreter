#lang racket

; Simple-interpreter HW1 Programming Language Concepts 2021
; Authors: Barry McCoy, James Kennelly, Paul Rodriguez

(require "simpleParser.rkt")

; atoms that are not allowed to be variables
(define forbidden-characters '(+ - / * % == != < <= > >= && || !)) 

; FUNCTION: takes a filename, calls parser with the filename, evaluates the syntax tree returned by parser, and returns the proper value
; INPUT: file with Java/C-ish code
; OUTPUT: value
(define interpret
  (lambda (filename)
   (read-syntax-tree (parser filename) create-new-state)))

; FUNCTION: takes in syntax tree and runs through each statement. returns the proper value
; INPUT: parse tree
; OUTPUT: proper value
(define read-syntax-tree
  (lambda (parse-tree state)
    (cond
      ((null? parse-tree) (get-var-value 'return-value state))
      ((list? (car parse-tree)) (read-syntax-tree (cdr parse-tree) (read-statement (car parse-tree) state)))
      (else (error 'invalid-statement)))))

(define read-syntax-tree-no-return
  (lambda (parse-tree state)
    (cond
      ((null? parse-tree) state)
      ((list? (car parse-tree)) (read-syntax-tree-no-return (cdr parse-tree) (read-statement (car parse-tree) state)))
      (else (error 'invalid-statement)))))

; FUNCTION: takes in a statement from the parse tree and evaluates it
; INPUT: statement
; OUTPUT: result of evaluated statement
(define read-statement
  (lambda (statement state)
    (cond
      ((null? statement) (get-var-value 'return-value state))
      ((and (eq? (car statement) 'var) (eq? (length statement) 3)) (M-state-var-define (parameter1 statement) (parameter2 statement) state))
      ((and (eq? (car statement) 'var) (eq? (length statement) 2)) (M-state-var-initialize (parameter1 statement) state))
      ((and (eq? (car statement) '=))                              (M-state-assign (parameter1 statement) (parameter2 statement) state))
      ((and (eq? (car statement) 'return))                         (M-state-return (parameter1 statement) state))
      ((and (eq? (car statement) 'if) (eq? (length statement) 4))  (M-state-if-else (parameter1 statement) (parameter2 statement) (parameter3 statement) state))
      ((and (eq? (car statement) 'if) (eq? (length statement) 3))  (M-state-if (parameter1 statement) (parameter2 statement) state))
      ((and (eq? (car statement) 'while))                          (M-state-while (parameter1 statement) (parameter2 statement) state))
      ((and (eq? (car statement) 'begin))                          (M-state-begin (cdr statement) state))
      (else                                                        (error 'invalid-statement)))))

; FUNCTION: returns second element of a list
; INPUT: expression
; OUTPUT: second element of the expression
(define parameter1
  (lambda (expression)
    (cadr expression)))

; FUNCTION: returns third element of a list
; INPUT: expression
; OUTPUT: third element of the expression
(define parameter2
  (lambda (expression)
    (caddr expression)))

; FUNCTION: returns fourth element of a list
; INPUT: expression
; OUTPUT: fourth element of the expression
(define parameter3
  (lambda (expression)
    (cadddr expression)))

; FUNCTION: evaluates the return statement
; INPUT: return statement and current state
; OUTPUT: updated state
(define M-state-return
  (lambda (expression state)
    (if (atom? expression)
        (cond
          ((integer? expression) (M-state-return-cleanup expression state))
          ((number? expression) (M-state-return-cleanup expression))
          (else (if (check-var (get-var-value expression state) state)
                    (M-state-return (get-var-value expression state) state)
                    (M-state-return-cleanup (get-var-value expression state) state))))
        (M-state-return-cleanup (M-value expression state) state))))

; FUNCTION: checks if input is an atom
; INPUT: x
; OUTPUT: boolean
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; FUNCTION: checks if input is a variable
; INPUT: x
; OUTPUT: boolean
(define variable?
  (lambda (x)
    (and (atom? x) (not (or (number? x) (contains? x forbidden-characters) (eq? x 'true) (eq? x 'false))))))

; FUNCTION: takes return value and checks that it is an integer or boolean
; INPUT: return value and state
; OUTPUT: proper return value
(define M-state-return-cleanup
  (lambda (output state)
    (cond
      ((integer? output) (update-value 'return-value output state))
      ((number? output) (exact-floor (update-value 'return-value output state)))
      ((eq? output #f) (update-value 'return-value 'false state))
      ((eq? output #t) (update-value 'return-value 'true state))
      ((variable? output) (if (check-var output state)
                              (update-value 'return-value (car (myreplace output (get-var-value output state) (list output))) state)
                              (error 'undefined-variable)))
      (else (error 'bad-output)))))

; FUNCTION: evaluates the assign statement
; INPUT: assign statement and current state
; OUTPUT: updated state
(define M-state-assign
  (lambda (var expression state)
    (if (check-var var state)
        (update-value var (M-value expression state) state)
        (error 'undefined-variable))))

; FUNCTION: evaluates the one parameter variable declaration statement
; INPUT: one parameter variable declaration statement and current state
; OUTPUT: updated state
(define M-state-var-define
  (lambda (var value state)
    (if (check-var var state)
        (error 'already-defined)
        (add-binding-pair var (M-value value state) state))))

; FUNCTION: evaluates the two parameter variable declaration statement
; INPUT: two parameter variable declaration statement and current state
; OUTPUT: updated state
(define M-state-var-initialize
  (lambda (var state)
    (add-binding-pair-var-only var state)))

; FUNCTION: evaluates the three parameter if statement
; INPUT: three parameter if statement and current state
; OUTPUT: updated state
(define M-state-if-else
  (lambda (cdal body else-statement state)
    (if (M-value cdal state)
        (read-statement body state)
        (read-statement else-statement state))))

; FUNCTION: evaluates the two parameter if statement
; INPUT: two parameter if statement and current state
; OUTPUT: updated state
(define M-state-if
  (lambda (cdal body state)
    (if (M-value cdal state)
        (read-statement body state)
        state)))

; FUNCTION: evaluates the while statement
; INPUT: while statement and current state
; OUTPUT: updated state
(define M-state-while
  (lambda (cdal body state)
    (if (M-value cdal state)
        (M-state-while cdal body (read-statement body state))
        state)))

(define M-state-begin
  (lambda (body state)
    (read-syntax-tree-no-return (cdr body) (read-statement (car body) (cons state create-new-state)))))

; FUNCTION: evaluates the value of any expression containing numbers and variables
; INPUT: expression and current state
; OUTPUT: updated state
(define M-value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((variable? expression) (if (check-var expression state)
                                  (car (myreplace (list expression) (get-var-value expression state) (list expression)))
                                  (error 'unassigned-variable)))
      ((eq? (operator expression) '+)      (+ (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((and (eq? (operator expression) '-) (eq? (length expression) 3) (- (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state))))
      ((and (eq? (operator expression) '-) (eq? (length expression) 2) (- 0 (M-value (leftoperand expression state) state))))
      ((eq? (operator expression) '/)      (quotient (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))                            
      ((eq? (operator expression) '*)      (* (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '%)      (remainder (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '==)     (eq? (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '!=)     (not (eq? (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state))))
      ((eq? (operator expression) '<)      (< (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '<=)     (<= (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '>)      (> (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '>=)     (>= (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '&&)     (and (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '||)     (or (M-value (leftoperand expression state) state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '!)      (not (M-value (leftoperand expression state) state)))
      (else (error 'bad-operator)))))

; FUNCTION: finds the operator of expression in prefix notation
; INPUT: expression
; OUTPUT: operator
(define operator
  (lambda (expression)
    (car expression)))

; FUNCTION: returns the left-hand side of expression
; INPUT: expression
; OUTPUT: left-hand side of expression
(define leftoperand
  (lambda (expression state)
    (cond
      ((list? (cadr expression)) (cadr expression))
      ((eq? (cadr expression) 'true) #t)
      ((eq? (cadr expression) 'false) #f)
      ((not (or (number? (cadr expression)) (contains? (cadr expression) forbidden-characters))) (if (check-var (cadr expression) state)
                                                                                                     (cadr (myreplace (cadr expression) (get-var-value (cadr expression) state) expression))
                                                                                                     (error 'undefined-variable)))
      (else (cadr expression)))))

; FUNCTION: returns the right-hand side of expression
; INPUT: expression
; OUTPUT: right-hand side of expression
(define rightoperand
  (lambda (expression state)
    (cond
      ((list? (caddr expression)) (caddr expression))
      ((eq? (caddr expression) 'true) #t)
      ((eq? (caddr expression) 'false) #f)
      ((not (or (number? (caddr expression)) (contains? (caddr expression) forbidden-characters))) (if (check-var (caddr expression) state)
                                                                                                     (caddr (myreplace (caddr expression) (get-var-value (caddr expression) state) expression))
                                                                                                     (error 'undefined-variable)))
      (else (caddr expression)))))

; FUNCTION: checks if list contains x
; INPUT: x
; OUTPUT: boolean
(define contains?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((eq? lis x) #t)
      (else (contains? x (cdr lis))))))

; FUNCTION: replaces all instances of a with b in list
; INPUT: a, b, list
; OUTPUT: updated list
(define myreplace
  (lambda (a b lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (myreplace a b (car lis)) (myreplace a b (cdr lis))))
      ((eq? (car lis) a) (cons b (myreplace a b (cdr lis))))
      (else (cons (car lis) (myreplace a b (cdr lis)))))))

;FUNCTION: add a variable without a value to list
;INPUT: a list and a new variable
;OUTPUT: the list that results from adding the variable
(define add-binding-pair-var-only
  (lambda (var state)
    (cons (list var) state)))

;FUNCTION: add a binding pair to list
;INPUT: a list, new variable, and the variable's value
;OUTPUT: the list that results from adding the binding pair
(define add-binding-pair
  (lambda (var val state)
    (cons (list var (box val)) state)))

;FUNCTION: check if the variable is in the binding pairs list
;INPUT: a list and variable
;OUTPUT: true if the variable is in the list and false if it is not
;(define check-var
;  (lambda (var state)
;    (cond ((null? state)                  #f)
;          ((equal? (car (car state)) var) #t)
;          ((list? (car (car state)))      (check-var var (car state)) (check-var var (cdr state)))
;          (else                           (check-var var (cdr state))))))

(define check-var-return
  (lambda (var state return)
    (cond ((null? state)                  (return #f))
          ((equal? (car (car state)) var) (return #t))
          ((list? (car (car state)))      (check-var-return var (cdr state) (lambda (v) (check-var-return var (car state) return))))
          (else                           (check-var-return var (cdr state) return)))))

(define check-var
  (lambda (var state)
    (check-var-return var state (lambda (v) v))))


;FUNCTION: gets value of variable from the list of binding pairs
;INPUT: a list and a variable
;OUTPUT: the value bound to the variable
;(define get-var-value
;  (lambda (var state)
;    (cond ((null? state)                                                        error 'error)
;          ((and (equal? 1 (length (car state))) (equal? (car (car state)) var)) error 'undeclared-variable)
;          ((equal? (car (car state)) var)                                       (unbox (cadr (car state))))
;          (else                                                                 (get-var-value var (cdr state))))))

(define get-var-value-return
  (lambda (var state return)
    (cond ((null? state)                                                        (return (error 'error)))
          ((and (equal? 1 (length (car state))) (equal? (car (car state)) var)) (return (error 'undeclared-variable)))
          ((equal? (caar state) var)                                            (return (unbox (cadr (car state)))))
          ((list? (caar state))                                                 (get-var-value-return var (car state) return))
          (else                                                                 (get-var-value-return var (cdr state) return)))))

(define get-var-value
  (lambda (var state)
    (get-var-value-return var state (lambda (v) v))))


;FUNCTION: update an old value bound with an existing variable or unassigned variable to a new value
;INPUT : a list, exisiting variable, and new value
;OUTPUT: the list that results from updating the variable's value
(define update-value
  (lambda (var val state)
    (cond ((null? state)                      state)
          ((eq? var val)                      state)
          ((and (eq? (length (car state)) 1)  (eq? var (car (car state)))) (cons (append (car state) (list (box val))) (cdr state)))
          ((equal? (caar state) var)          (cons (list var (box val)) (cdr state)))
          ((list? (caar state))               (cons (update-value var val (car state)) (update-value var val (cdr state))))
          (else                               (cons (car state) (update-value var val (cdr state)))))))

;FUNCTION: creates a new state with a default reutn value of 0
;OUTPUT: a new state
;(define create-new-state '((return-value (box 0))))

(define create-new-state (list (cons 'return-value (list (box 0)))))
