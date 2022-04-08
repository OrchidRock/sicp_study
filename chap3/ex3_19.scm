;:
;: exercise 3.19
;:

(define (ring? pairs)
    (define (iter x deep)
        (if (or (null? x) (not (pair? x)))
            deep
            (let ((left-deep))))))
