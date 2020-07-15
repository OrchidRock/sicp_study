;:
;: exercise 2.27
;: To produce a procedure that takes a list as argument and returns as its value the 
;: list with its elements reversed and with all sublists deep-reversed as well.
;:

(load "reverse.scm")

 (define (deep-reverse tree)
    (define (iter tree result)
        (cond ((null? tree) result)
              ((not (pair? tree)) tree)
              (else (iter (cdr tree) 
                          (cons (iter (car tree) '()) result)))))
    (iter tree '()))

;: test

(define x (list (list 1 2) (list 3 4)))
(define x2 (list (list 1 (list 3 4)) 2 (list 5 6)))
