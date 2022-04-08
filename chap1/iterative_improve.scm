;:
;: exercise 1.46
;:

(define (iterative-improve guess-ok guess-improve)
    (define (iter k)
        (if (guess-ok k (guess-improve k))
            k
            (iter (guess-improve k))))
    (lambda (x) (iter x)))

(define (fix-point-ii f first-guess)
    ((iterative-improve (lambda (x y) (< (abs (- x y))
                                        tolerance))
                        f)
     first-guess))
