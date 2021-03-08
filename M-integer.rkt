#lang racket

(require "simpleParser.rkt")

(define M-eval
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((eq? (operator expression) '+) (+ (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((and (eq? (operator expression) '-) (eq? (length expression) 3) (- (M-eval (leftoperand expression)) (M-eval (rightoperand expression)))))
      ((and (eq? (operator expression) '-) (eq? (length expression) 2) (- 0 (M-eval (leftoperand expression)))))
      ((eq? (operator expression) '/) (quotient (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))                            
      ((eq? (operator expression) '*) (* (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '==) (eq? (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '!=) (not (eq? (M-eval (leftoperand expression)) (M-eval (rightoperand expression)))))
      ((eq? (operator expression) '<) (< (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '<=) (<= (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '>) (> (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '>=) (>= (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '&&) (and (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '||) (or (M-eval (leftoperand expression)) (M-eval (rightoperand expression))))
      ((eq? (operator expression) '!) (not (M-eval (leftoperand expression))))
      (else (error 'bad-operator)))))

(define operator
  (lambda (expression)
    (car expression)))

(define leftoperand
  (lambda (expression)
    (cadr expression)))

(define rightoperand
  (lambda (expression)
    (caddr expression)))


(define read-parse-tree
  (lambda (parse-tree M-state)
    (cond
      ((null? parse-tree) 'end)
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'var) (eq? (length (car parse-tree) 3))) (var-define-parse (cadar parse-tree) (cddar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'var) (eq? (length (car parse-tree) 2))) (var-initialize-parse (cadar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) '=)) (assign-parse (cadar parse-tree) (cddar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'return)) (return-parse (cadar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'if) (eq? (length (car parse-tree) 4))) (if-else-parse (cadar parse-tree) (caddar parse-tree) (cdddar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'if) (eq? (length (car parse-tree) 3))) (if-parse (cadar parse-tree) (cddar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'while)) (while-parse (cadar parse-tree) (cddar parse-tree) M-state) (read-parse-tree (cdr parse-tree)))
      (else (read-parse-tree (cdr parse-tree))))))

(define return-parse
  (lambda (expression)
    (return-parse-cleanup (M-eval expression))))

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
    ;replace all vars with their values
    (add-binding-pair state var (M-eval expression))))

(define var-define-parse
  (lambda (var value state)
    (add-binding-pair state var value)))

(define var-initialize-parse
  (lambda (var state)
    (add-binding-pair-var-only state var)))

(define if-parse
  (lambda (conditional body state)
    ;replace all vars with their values
    (if (M-eval conditional)
        (read-parse-tree body)
        #f)))

(define if-else-parse
  (lambda (cdal body else-statement state)
    ;replace all vars with their values
    (if (M-eval cdal)
        (read-parse-tree body)
        (read-parse-tree else-statement))))

(define while-parse
  (lambda (cdal body state)
    ;replace all vars with their values
    (if (M-eval cdal)
        ((read-parse-tree body) (while-parse cdal body))
        #f)))

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