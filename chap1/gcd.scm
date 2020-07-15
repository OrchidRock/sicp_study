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
