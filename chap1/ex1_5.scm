;: To test the substitution model which interpreter has used.
;: applicative-order: dead loop
;: normal-order     : return value 0
;:      (test 0 (p))
;:      (if (= 0 0)
;:          0
;:          (p))

(define (p) (p))

(define (test x y)
    (if (= x 0)
        0
        y))

;: Test
;:(test 0 (p))
