;:
;: Given a positive integer n, find all ordered pairs of distinct positive
;: integers i and j, where 1<= j < i <= n, such that i + j is a prime.
;:

(load "conventional_interfaces.scm")
(cd "../chap1")
(load "prime.scm")
(cd "../chap2")

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (car (cdr pair)))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(load "unique_pairs.scm")
(define (prime-pair-sum n)
    (map make-pair-sum 
         (filter prime-sum? (unique-pairs n))))

;: Test
(define T1 (prime-pair-sum 5))
