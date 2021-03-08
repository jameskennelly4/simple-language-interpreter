#lang racket

;(require "simpleParser.rkt")

(define forbidden-characters '(+ - / * % == != < <= > >= && || !))

(define M-eval
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((eq? (operator expression) '+)      (+ (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((and (eq? (operator expression) '-) (eq? (length expression) 3) (- (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state))))
      ((and (eq? (operator expression) '-) (eq? (length expression state) 2) (- 0 (M-eval (leftoperand expression state) state))))
      ((eq? (operator expression) '/)      (quotient (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))                            
      ((eq? (operator expression) '*)      (* (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '%)      (remainder (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '==)     (eq? (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '!=)     (not (eq? (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state))))
      ((eq? (operator expression) '<)      (< (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '<=)     (<= (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '>)      (> (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '>=)     (>= (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '&&)     (and (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '||)     (or (M-eval (leftoperand expression state) state) (M-eval (rightoperand expression state) state)))
      ((eq? (operator expression) '!)      (not (M-eval (leftoperand expression state) state)))
      (else (error 'bad-operator)))))

(define operator
  (lambda (expression)
    (car expression)))

(define leftoperand
  (lambda (expression state)
    (cond
      ((list? (cadr expression)) (cadr expression))
      ((not (or (number? (cadr expression)) (contains? (cadr expression) forbidden-characters))) (if (check-var state (cadr expression))
                                                                                                     (cadr (myreplace (cadr expression) (get-var-value state (cadr expression)) expression))
                                                                                                     (error 'undefined-variable)))
      (else (cadr expression)))))

(define rightoperand
  (lambda (expression state)
    (cond
      ((list? (caddr expression)) (caddr expression))
      ((not (or (number? (caddr expression)) (contains? (caddr expression) forbidden-characters))) (if (check-var state (caddr expression))
                                                                                                     (caddr (myreplace (caddr expression) (get-var-value state (caddr expression)) expression))
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

(define parameter1
  (lambda (expression)
    (cadr expression)))

(define parameter2
  (lambda (expression)
    (caddr expression)))

(define parameter3
  (lambda (expression)
    (cadddr expression)))

(define read-parse
  (lambda (statement M-state)
    (display M-state) (newline)
    (cond
      ((null? statement) 'end)
      ((and (eq? (car statement) 'var) (eq? (length statement) 3)) (var-define-parse (parameter1 statement) (parameter2 statement) M-state))
      ((and (eq? (car statement) 'var) (eq? (length statement) 2)) (var-initialize-parse (parameter1 statement) M-state))
      ((and (eq? (car statement) '=)) (assign-parse (parameter1 statement) (parameter2 statement) M-state))
      ((and (eq? (car statement) 'return)) (return-parse (parameter1 statement) M-state))
      ((and (eq? (car statement) 'if) (eq? (length statement) 4)) (if-else-parse (parameter1 statement) (parameter2 statement) (parameter3 statement) M-state))
      ((and (eq? (car statement) 'while)) (while-parse (cadr statement) (caddr statement) M-state))
      (else (error 'invalid-statement)))))

(define read-parse-tree
  (lambda (parse-tree M-state)
    (display M-state) (newline)
    (cond
      ((null? parse-tree) 'end)
      ((list? (car parse-tree)) (read-parse-tree (cdr parse-tree) (read-parse (car parse-tree) M-state)))
      (else (error 'invalid-statement)))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

(define return-parse
  (lambda (expression state)
    (if (atom? expression)
        (cond
          ((integer? expression) (return-parse-cleanup expression))
          ((number? expression) (return-parse-cleanup expression))
          (else (return-parse-cleanup (get-var-value state expression))))
        (return-parse-cleanup (M-eval expression state)))))

(define return-parse-cleanup
  (lambda (output)
    (cond
      ((integer? output) output)
      ((number? output) (exact-floor output))
      ((eq? output #f) 'false)
      ((eq? output #t) 'true)
      (else (error 'bad-output)))))

(define assign-parse
  (lambda (var expression state)
    (replace-old-value state var (M-eval expression state))))

(define var-define-parse
  (lambda (var value state)
    (add-binding-pair state var (M-eval value state))))

(define var-initialize-parse
  (lambda (var state)
    (add-binding-pair-var-only state var)))

(define if-else-parse
  (lambda (cdal body else-statement state)
    (if (M-eval cdal state)
        (read-parse body state)
        (read-parse else-statement state))))

(define while-parse
  (lambda (cdal body state)
    (if (M-eval cdal state)
        (while-parse cdal body (read-parse body state))
        state)))

;FUNCTION: add a variable without a value to list
;INPUT: a list and a new variable
;OUTPUT: the list that results from adding the variable
(define add-binding-pair-var-only
  (lambda (pairs var)
    (cons (list var) pairs)))

;FUNCTION: add a binding pair to list
;INPUT: a list, new variable, and the variable's value
;OUTPUT: the list that results from adding the binding pair
(define add-binding-pair
  (lambda (pairs var val)
    (cons (list var val) pairs)))

;FUNCTION: check if the variable is in the binding pairs list
;INPUT: a list and variable
;OUTPUT: true if the variable is in the list and false if it is not
(define check-var
  (lambda (pairs var)
    (cond ((null? pairs)                  #f)
          ((equal? (car (car pairs)) var) #t)
          (else                           (check-var (cdr pairs) var)))))

;FUNCTION: gets value of variable from the list of binding pairs
;INPUT: a list and a variable
;OUTPUT: the value bound to the variable
(define get-var-value
  (lambda (pairs var)
    (cond ((null? pairs)                                                        error 'error)
          ((and (equal? 1 (length (car pairs))) (equal? (car (car pairs)) var)) error 'undeclared-variable)
          ((equal? (car (car pairs)) var)                                       (cadr (car pairs)))
          (else                                                                 (get-var-value (cdr pairs) var))))) 

;FUNCTION: replace an old value bound with an existing variable to a new value
;INPUT : a list, exisiting variable, and new value
;OUTPUT: the list that results from replacing the old value with the new value
(define replace-old-value
  (lambda (pairs var val)
    (cond ((null? pairs)                  error 'error)
          ((and (eq? (length (car pairs)) 1) (eq? var (car (car pairs)))) (cons (append (car pairs) (list val)) (cdr pairs)))
          ((equal? (car (car pairs)) var) (cons (list var val) (cdr pairs)))
          (else                           (cons (car pairs) (replace-old-value (cdr pairs) var val))))))

