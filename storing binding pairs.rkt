#lang racket

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
          ((equal? (car (car pairs)) var) (cons (list var val) (cdr pairs)))
          (else                           (cons (car pairs) (replace-old-value (cdr pairs) var val))))))
