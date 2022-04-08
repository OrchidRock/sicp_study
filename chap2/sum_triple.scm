;:
;: exercise 2.41
;: Write a procedure to find all ordered tripies of distinct positive
;: integers i, j and k less than or equal to a given integer n that sum to a given integer s.
;:
(load "permutations.scm")
(load "unique_triple.scm")
(define (sum-triple n s)
    (define (sum-s-triple? t)
        (= (+ (car t) (cadr t) (caddr t)) s))
    (accumulate append
                '()
                (map (lambda (t) (permutations t))
                     (filter sum-s-triple? (unique-triple n)))))
;: Test
(define T0 (unique-triple 5))
(define T1 (sum-triple 5 6))
(define T2 (sum-triple 5 7))
(define T3 (sum-triple 5 8))
(define T4 (sum-triple 5 9))
(define T5 (sum-triple 5 10))
(define T6 (sum-triple 5 11))
(define T7 (sum-triple 5 12))
