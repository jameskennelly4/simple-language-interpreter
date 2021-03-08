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
  (lambda (parse-tree)
    (cond
      ((null? parse-tree) 'end)
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'var) (eq? (length (car parse-tree) 3))) (var-define-parse (cadar parse-tree) (cddar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'var) (eq? (length (car parse-tree) 2))) (var-initialize-parse (cadar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) '=)) (assign-parse (cadar parse-tree) (cddar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'return)) (return-parse (cadar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'if) (eq? (length (car parse-tree) 4))) (if-else-parse (cadar parse-tree) (caddar parse-tree) (cdddar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'if) (eq? (length (car parse-tree) 3))) (if-parse (cadar parse-tree) (cddar parse-tree)) (read-parse-tree (cdr parse-tree)))
      ((and (list? (car parse-tree)) (eq? (caar parse-tree) 'while)) (while-parse (cadar parse-tree) (cddar parse-tree) (read-parse-tree (cdr parse-tree))))
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
  (lambda (var expression)
    (add-binding-pair pairs var (M-eval expression))))

(define var-define-parse
  (lambda (var value)
    (add-binding-pair pairs var val)))

(define var-initialize-parse
  (lambda (var)
    (add-binding-pair-var-only pairs var)))

(define if-parse
  (lambda (conditional body)
    (if (M-eval conditional)
        (read-parse-tree body)
        #f)))

(define if-else-parse
  (lambda (condtional body else-statement)
    (if (M-eval conditional)
        (read-parse-tree body)
        (read-parse-tree else-statement))))

(define while-parse
  (lambda (condtional body)
    (if (M-eval conditional)
        ((read-parse-tree body) (while-parse conditional body)))))