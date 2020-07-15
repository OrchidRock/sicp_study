;:
;: exercise 2.38
;:
;:
(load "conventional_interfaces.scm")

(define fold-right accumulate)

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(define T1 (fold-right / 1 (list 1 2 3)))
(define T2 (fold-left / 1 (list 1 2 3)))

(define T3 (fold-right list '() (list 1 2 3)))
(define T4 (fold-left list '() (list 1 2 3)))

;: What a property that op should satisfy to guarantee that fold-right and fold-left will produce 
;: the same value for any sequence?
;: Answer: op should satisfy law of commutation, such as * or +, etc.
;:
