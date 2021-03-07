#lang racket

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