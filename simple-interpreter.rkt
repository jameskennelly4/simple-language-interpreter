#lang racket

(require "simpleParser.rkt")

(define forbidden-characters '(+ - / * % == != < <= > >= && || !))

(define interpret
  (lambda (filename)
   (read-syntax-tree (parser filename) create-new-state)))

(define read-syntax-tree
  (lambda (parse-tree state)
    (cond
      ((null? parse-tree) (get-var-value 'return-value state))
      ((list? (car parse-tree)) (read-syntax-tree (cdr parse-tree) (read-statement (car parse-tree) state)))
      (else (error 'invalid-statement)))))

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
      (else                                                        (error 'invalid-statement)))))

(define parameter1
  (lambda (expression)
    (cadr expression)))

(define parameter2
  (lambda (expression)
    (caddr expression)))

;James Kennelly
(define parameter3
  (lambda (expression)
    (cadddr expression)))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

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

(define M-state-return-cleanup
  (lambda (output state)
    (cond
      ((integer? output) (update-value 'return-value output state))
      ((number? output) (exact-floor (update-value 'return-value output state)))
      ((eq? output #f) (update-value 'return-value 'false state))
      ((eq? output #t) (update-value 'return-value 'true state))
      ((and (atom? output) (not (or (number? output) (contains? output forbidden-characters)))) (if (check-var output state)
                                                                                                                (update-value 'return-value (car (myreplace output (get-var-value output state) (list output))) state)
                                                                                                                (error 'undefined-variable)))
      (else (error 'bad-output)))))

(define M-state-assign
  (lambda (var expression state)
    (update-value var (M-value expression state) state)))

(define M-state-var-define
  (lambda (var value state)
    (add-binding-pair var (M-value value state) state)))

(define M-state-var-initialize
  (lambda (var state)
    (add-binding-pair-var-only var state)))

(define M-state-if-else
  (lambda (cdal body else-statement state)
    (if (M-value cdal state)
        (read-statement body state)
        (read-statement else-statement state))))

(define M-state-if
  (lambda (cdal body state)
    (if (M-value cdal state)
        (read-statement body state)
        state)))

(define M-state-while
  (lambda (cdal body state)
    (if (M-value cdal state)
        (M-state-while cdal body (read-statement body state))
        state)))

(define M-value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((and (atom? expression) (not (or (number? expression) (contains? expression forbidden-characters) (eq? expression 'true) (eq? expression 'false)))) (if (check-var expression state)
                                                                                                                (car (myreplace (list expression) (get-var-value expression state) (list expression)))
                                                                                                                (error 'undefined-variable)))
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

(define operator
  (lambda (expression)
    (car expression)))

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

(define contains?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((eq? lis x) #t)
      (else (contains? x (cdr lis))))))

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
    (cons (list var val) state)))

;FUNCTION: check if the variable is in the binding pairs list
;INPUT: a list and variable
;OUTPUT: true if the variable is in the list and false if it is not
(define check-var
  (lambda (var state)
    (cond ((null? state)                  #f)
          ((equal? (car (car state)) var) #t)
          (else                           (check-var var (cdr state))))))

;FUNCTION: gets value of variable from the list of binding pairs
;INPUT: a list and a variable
;OUTPUT: the value bound to the variable
(define get-var-value
  (lambda (var state)
    (cond ((null? state)                                                        error 'error)
          ((and (equal? 1 (length (car state))) (equal? (car (car state)) var)) error 'undeclared-variable)
          ((equal? (car (car state)) var)                                       (cadr (car state)))
          (else                                                                 (get-var-value var (cdr state)))))) 

;FUNCTION: update an old value bound with an existing variable or unassigned variable to a new value
;INPUT : a list, exisiting variable, and new value
;OUTPUT: the list that results from updating the variable's value
(define update-value
  (lambda (var val state)
    (cond ((null? state)                     error 'error)
          ((eq? var val)                     state)
          ((and (eq? (length (car state)) 1) (eq? var (car (car state)))) (cons (append (car state) (list val)) (cdr state)))
          ((equal? (car (car state)) var)    (cons (list var val) (cdr state)))
          (else                              (cons (car state) (update-value var val (cdr state)))))))

;FUNCTION: creates a new state with a default reutn value of 0
;OUTPUT: a new state
(define create-new-state '((return-value 0)))
