;:
;: exercise 2.41
;: Write a procedure to find all ordered tripies of distinct positive
;: integers i, j and k less than or equal to a given integer n that sum to a given integer s.
;:
(load "permutations.scm")
(load "unique_tripies.scm")
(define (sum-tripies n s)
    (define (sum-s-tripies? t)
        (= (+ (car t) (cadr t) (caddr t)) s))
    (accumulate append 
                '()
                (map (lambda (t) (permutations t))
                     (filter sum-s-tripies? (unique-tripies n)))))
;: Test
(define T0 (unique-tripies 5))
(define T1 (sum-tripies 5 6))
(define T2 (sum-tripies 5 7))
(define T3 (sum-tripies 5 8))
(define T4 (sum-tripies 5 9))
(define T5 (sum-tripies 5 10))
(define T6 (sum-tripies 5 11))
(define T7 (sum-tripies 5 12))
