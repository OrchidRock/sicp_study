;:
;:
;:

(load "streams.scm")

(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
                     (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
    guesses)

;: We can treat it in the same way is to generate an approximation to Pi
;: ,based upon the Leibnitz alternating series.

(define (pi-summands n)
    (cons-stream (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))
(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

;: sequence accelerator 
(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
        (cons-stream (- s2 (/ (square (- s2 s1))
                             (+ s0 (* -2 s1) s2)))
                     (euler-transform (stream-cdr s)))))

;: tableau
(define (make-tableau transform s)
    (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
    (stream-map stream-car (make-tableau transform s)))

;: exercise 3.64
(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

;: exercise 3.65
(define (ln2-summands n)
    (cons-stream (/ 1.0 n) (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream (partial-sums (ln2-summands 1)))
(define accelerated-ln2-stream (euler-transform ln2-stream))
(define super-accelerated-ln2-stream (accelerated-sequence euler-transform ln2-stream))

;: Test

(define sqrt2 (sqrt-stream 2))
(define accelerated-pi-stream (euler-transform pi-stream))
(define super-accelerated-pi-stream (accelerated-sequence euler-transform pi-stream))

(define t1 (sqrt 2 0.01))

