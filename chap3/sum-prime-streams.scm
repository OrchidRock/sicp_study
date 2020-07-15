;:
;:
;:
(load "streams.scm")
(cd "../chap1")
(load "prime.scm")
(cd "../chap2")
(load "conventional_interfaces.scm")
(cd "../chap3")

(define (sum-primes a b)
    (fold-left + 0 (filter prime? (enumerate-interval a b))))

;: Test
;: Value: 10009
(define T1 (lambda () (car (cdr (filter prime? 
                             (enumerate-interval 10000 100000))))))


(define T2 (stream-enumerate-interval 1 10))
(define T3 (lambda () (stream-car (stream-cdr (stream-filter prime?
                                                  (stream-enumerate-interval 10000 100000))))))
