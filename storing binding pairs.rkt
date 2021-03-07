; FUNCTION: gets value from list of binding pairs
; INPUT: a list and a variable
; OUTPUT: the value bound with the variable or 'error
(define (get-var-value pairs x)
  (cond ((null? pairs) 'error)
        ((equal? (car (car pairs)) x) (cadr (car pairs)))
        (else (get-var-value (cdr pairs) x))))

; FUNCTION: check if the variable in the binding pairs
; INPUT : a list and variable
; OUTPUT : true if the variable is in the map and false otherwise
(define (check-var pairs x)
  (cond ((null? pairs) #f)
        ((equal? (car (car pairs)) x) #t)
        (else (check-var (cdr pairs) x))))