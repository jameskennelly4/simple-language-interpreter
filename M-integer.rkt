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

(define return-parse
  (lambda (output)
    (cond
      ((integer? output) output)
      ((number? output) (exact-floor output))
      ((eq? output #f) 'false)
      ((eq? output #t) 'true)
      (else (error 'bad-output)))))

(define read-parse-tree
  (lambda (parse-tree)
    (cond
      ((null? parse-tree) 'end)
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'var)) (var-parse) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) '=)) (assign-parse (cadar parse-tree) (cddar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'return)) (return-parse (cadar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'if)) (if-parse) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'while)) (while-parse (cadar parse-tree) (cddar parse-tree) (read-parse-tree (cdr parse-tree))))
      (else (read-parse-tree (cdr parse-tree))))))

(define print-output
  (lambda (out)
    (display out) (newline)))