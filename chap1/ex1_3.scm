;: To define a function which has three parameters and return the sum of
;: largest parameter and less largest parameter.

(define (ex1_3_f a b c)
    (if (< a b)
        (if (< a c)
            (+ b c)
            (+ a b))
        (if (< b c)
            (+ a c)
            (+ a b))))

;; Test instances
;; (ex1_3_f 23 23 23)
;; (ex1_3_f 3 54 6)
;; (ex1_3_f 32 45 65)
;; (ex1_3_f 65 32 4)
;; (ex1_3_f 32 3 33)

