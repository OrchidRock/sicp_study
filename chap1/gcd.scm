;:
;: gcd.scm
;: Get the gcd of two integers by Euclidi's algorithm.
;:

(declare (usual-integrations))


;: iterative process

(define (gcd-my a b)
    (define (gcd-iter a b)
        (if (= b 0)
            a
            (gcd-iter b (remainder a b))))
    (if (< a b)
        (gcd-iter b a)
        (gcd-iter a b)))

;: we have not to compare the value of a and b.

(define (gcd-my2 a b)
    (if (= b 0)
        a
        (gcd-my2 b (remainder a b))))
