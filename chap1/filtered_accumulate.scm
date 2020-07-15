;:
;: exercise 1.33
;: an even more general version of accumulate by introducing the notion of a
;: filter on the terms to be combined.


(define (filtered-accumulate filter? combiner null-value term a next b)
    (cond ((> a b) null-value)
          ((filter? a) (combiner (term a) (filtered-accumulate filter?
                                                                 combiner
                                                                 null-value
                                                                 term
                                                                 (next a)
                                                                 next
                                                                 b)))
          (else (combiner null-value (filtered-accumulate filter?
                                                        combiner
                                                        null-value
                                                        term
                                                        (next a)
                                                        next
                                                        b)))))

;: appliaction
(load "prime.scm")

;: a)
(define (sum-primes a b)
    (define (identity x) x)
    (define (next x) (+ x 1))
    (filtered-accumulate prime? + 0 identity a next b))

;: b)
(define (sum-relatively-prime n)
    (define (identity x) x)
    (define (next x) (+ x 1))
    (define (relatively-prime? x) (= (gcd x n) 1))
    (filtered-accumulate relatively-prime? * 1 identity 1 next n))
