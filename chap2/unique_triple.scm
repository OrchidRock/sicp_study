;:
;:
;:
(load "unique_pairs.scm")
(define (unique-triple n)
    (accumulate append
                '()
                (map (lambda (i) (map (lambda (x) (cons i x))
                                      (unique-pairs (- i 1))))
                    (enumerate-interval 1 n))))
;: Test
(define T1 (unique-tripies 3))
(define T2 (unique-tripies 4))
(define T3 (unique-tripies 5))
