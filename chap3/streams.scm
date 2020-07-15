;:
;:
;:


;: streams
;: cons-stream must be a special form, but I don't know how to define it.
;(define cons-stream 
;    (lambda (a b) (cons a (delay b)))) ; not working!!!

;: MIT/GNU Scheme supports a higher-level abstraction called streams
;(define cons-stream ())
;(define (stream-car s) (car s))
;(define (stream-cdr s) (force (cdr s)))
;(define the-empty-stream '())
;(define stream-null? null?)

;(define (stream-ref s n)
;  (if (= n 0)
;      (stream-car s)
;      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))
;

;: execise 3.50
;(define (stream-map proc . argstreams)
;    (if (stream-null? (car argstreams))
;        the-empty-stream
;        (cons-stream (apply proc (map stream-car argstreams))
;               (apply stream-map proc (map stream-cdr argstreams)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;(define (stream-filter pred stream)
;  (cond ((stream-null? stream) the-empty-stream)
;        ((pred (stream-car stream))
;         (cons-stream (stream-car stream)
;                      (stream-filter pred
;                                     (stream-cdr stream))))
;        (else (stream-filter pred (stream-cdr stream)))))

;: some streams
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (integers-recip-starting-from n)
    (cons-stream (/ 1 n) (integers-recip-starting-from (+ n 1))))
(define integers-recip (integers-recip-starting-from 1))


(define ones (cons-stream 1 ones))

(define (add-stream s1 s2)
    (stream-map + s1 s2))

(define integers2 (cons-stream 1 (add-stream ones integers2)))

(define fibs
    (cons-stream 0 (cons-stream 1 (add-stream (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor))
                stream))
(define double (cons-stream 1 (scale-stream double 2)))

;: exercise 3.53
(define double2 (cons-stream 1 (add-stream double2 double2)))

;: exercise 3.54
(define (mul-stream s1 s2)
    (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-stream factorials 
                                              (integers-starting-from 2))))

;: exercise 3.55
(define (partial-sums stream)
    (cons-stream (stream-car stream) (add-stream (partial-sums stream)
                                                 (stream-cdr stream))))
(define sum-of-integers (partial-sums integers))

;: exercise 3.56
(define (merge-stream s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else 
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                        (cons-stream s1car (merge-stream (stream-cdr s1) s2)))
                      ((> s1car s2car)
                        (cons-stream s2car (merge-stream s1 (stream-cdr s2))))
                      (else
                        (cons-stream s1car (merge-stream (stream-cdr s1)
                                                         (stream-cdr s2)))))))))
;: exercise 3.64
(define (stream-limit s tolerance)
    (let ((a0 (stream-ref s 0))
          (a1 (stream-ref s 1)))
        (if (< (abs (- a0 a1)) tolerance)
            a1
            (stream-limit (stream-cdr s) tolerance))))

;: Test
(define T2 (stream-enumerate-interval 1 10))
(define T3 (lambda () (stream-car (stream-cdr (stream-filter prime?
                                                  (stream-enumerate-interval 10000 100000))))))

(define s1 (stream-enumerate-interval 2 5))
(define s2 (stream-enumerate-interval 3 6))
(define t4 (stream-map + s1 s2))
