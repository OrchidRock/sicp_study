;
;:
;:

(load "streams_interfaces.scm")
(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                    (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                    (pairs (stream-cdr s) (stream-cdr t)))))

(define integer-pairs (pairs integers integers))

;: exercise 3.66
(define (pairs2 s t)
    (cons-stream (list (stream-car s) (stream-car t))
        (interleave (stream-map (lambda (x)
                                    (list (stream-car s) x))
                                (stream-cdr t))
                    (interleave (stream-map (lambda (x)
                                              (list x (stream-car t)))
                                            (stream-cdr s))
                                (pairs2 (stream-cdr s) (stream-cdr t))))))

(define integer-pairs2 (pairs2 integers integers))

;: there are some repeated pairs!!!
(define (reverse-pairs x)
    (stream-map (lambda (p) (list (cadr p) (car p))) x))
(define (pairs3 s t)
    (interleave (pairs s t)
                (reverse-pairs (pairs t (stream-cdr s)))))
(define integer-pairs3 (pairs3 integers integers))
;: exercise 3.69

(define (triples s t u)
    (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
                 (interleave (stream-map (lambda (x) (cons (stream-car s) x))
                                         (pairs t (stream-cdr u)))
                             (triples (stream-cdr s)
                                      (stream-cdr t)
                                      (stream-cdr u)))))

(define integer-triples (triples integers integers integers))

;:
(define pythagoras-triples
    (stream-filter (lambda (x)
                        (and (> (+ (car x) (cadr x))
                                (caddr x))
                             (= (+ (square (car x))
                                   (square (cadr x)))
                                (square (caddr x)))))
                    integer-triples))

;: exercise 3.70

(define (merge-weighted s1 s2 weight)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1car-weight (weight (stream-car s1)))
                  (s2car-weight (weight (stream-car s2))))
                (cond ((< s1car-weight s2car-weight)
                        (cons-stream (stream-car s1) (merge-weighted (stream-cdr s1) s2 weight)))
                      ((> s1car-weight s2car-weight)
                        (cons-stream (stream-car s2) (merge-weighted s1 (stream-cdr s2) weight)))
                      (else
                        (cons-stream (stream-car s1) (merge-weighted (stream-cdr s1)
                                                                     s2
                                                                     weight))))))))
(define (weighted-pairs s t weight)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (merge-weighted (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                        (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                        weight)))
;: a)
(define test-a (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
;: b)
(define (remainder235-to-0? x)
    (or (= (remainder x 2) 0)
        (= (remainder x 3) 0)
        (= (remainder x 5) 0)))

(define test-b (weighted-pairs (stream-filter remainder235-to-0? integers)
                               (stream-filter remainder235-to-0? integers)
                               (lambda (x) (+ (* 2 (car x))
                                              (* 3 (cadr x))
                                              (* 5 (* (car x) (cadr x)))))))

;: exercise 3.71
(define (cube x) (* x x x))
(define (filter-pairs s)
    (let ((x (stream-ref s 0))
          (y (stream-ref s 1)))
        (if (= (weight x) (weight y))
            (cons-stream (list x (list (weight x))) (filter-pairs (stream-cdr (stream-cdr s))))
            (filter-pairs (stream-cdr s)))))
(define (weight x)
    (+ (cube (car x)) (cube (cadr x))))

(define remanujan-number-stream
    (filter-pairs (weighted-pairs integers integers weight)))

;; exercise 3.72

(define (weight p)
    (+ (square (car p)) (square (cadr p))))
(define (filter-pairs s)
    (let ((x (stream-ref s 0))
          (y (stream-ref s 1))
          (z (stream-ref s 2)))
        (if (= (weight x) (weight y) (weight z))
            (cons-stream (list x y z (list (weight x)))
                (filter-pairs (stream-cdr (stream-cdr (stream-cdr s)))))
            (filter-pairs (stream-cdr s)))))
(define three-squ-number
    (filter-pairs (weighted-pairs integers integers weight)))
