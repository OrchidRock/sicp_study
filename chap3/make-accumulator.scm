;:
;: exercise 3.1 
;:

(define (make-acumulator init-value)
    (lambda (number)
        (begin (set! init-value (+ init-value number))
               init-value)))

;: Test

(define a1 (make-acumulator 5))
