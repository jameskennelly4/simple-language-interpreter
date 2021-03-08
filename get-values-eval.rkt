#lang racket

(define M-state '((x 10) (y 35)))
(define forbidden-characters '(+ - / * % == != < <= > >= && || !))

(define M-eval
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((eq? (operator expression) '+) (+ (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((and (eq? (operator expression) '-) (eq? (length expression) 3) (- (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state)))))
      ((and (eq? (operator expression) '-) (eq? (length expression) 2) (- 0 (M-eval (leftoperand expression state)))))
      ((eq? (operator expression) '/) (quotient (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))                            
      ((eq? (operator expression) '*) (* (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '%) (remainder (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '==) (eq? (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '!=) (not (eq? (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state)))))
      ((eq? (operator expression) '<) (< (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '<=) (<= (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '>) (> (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '>=) (>= (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '&&) (and (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '||) (or (M-eval (leftoperand expression state)) (M-eval (rightoperand expression state))))
      ((eq? (operator expression) '!) (not (M-eval (leftoperand expression state))))
      (else (error 'bad-operator)))))

(define operator
  (lambda (expression state)
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

(define get-var-value
  (lambda (pairs var)
    (cond ((null? pairs)                                                        error 'error)
          ((and (equal? 1 (length (car pairs))) (equal? (car (car pairs)) var)) error 'undeclared-variable)
          ((equal? (car (car pairs)) var)                                       (cadr (car pairs)))
          (else                                                                 (get-var-value (cdr pairs) var))))) 

(define check-var
  (lambda (pairs var)
    (cond ((null? pairs)                  #f)
          ((equal? (car (car pairs)) var) #t)
          (else                           (check-var (cdr pairs) var)))))
