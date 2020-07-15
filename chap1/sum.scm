;:
;: Formulating abstractions with Higher-Order procedures.
;:

;: sum is a procedures which has taken two procedures as it's arguments.
;: recursive version.
;:
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))


;: iterative version.
;: exercise 1.30
(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))

    (iter a 0))


;: load accumulate.scm to revoke another sum which build on accumulate procedure.
;: load procedure will override procedures above all.
(load "accumulate.scm")

;: pi's infinite series which converges to pi/8 (very solwly).
;: 
(define (pi-sum b)
    (define (pi-term x) (/ 1.0 (* x (+ x 2))))
    (define (pi-next x) (+ x 4))
    (* 8 (sum pi-term 1 pi-next b)))

(define (pi-sum-iter b)
    (define (pi-term x) (/ 1.0 (* x (+ x 2))))
    (define (pi-next x) (+ x 4))
    (* 8 (sum-iter pi-term 1 pi-next b)))

;: the definite integral of a  function f between the limits of a and b.
;: recursive version.
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;: iterative version.
(define (integral-iter f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum-iter f (+ a (/ dx 2.0)) add-dx b) dx))

;: exercise 1.29
;: Simpson's Rule is a more accurate method of numerical integration than the method 
;: illustarted above.

(define (integral-simpson f a b n)
    (define h (/ (- b a) n))
    (define (term k) 
        (cond ((= k 0) (f (+ a (* k h))))
              ((= k n) (f (+ a (* k h))))
              ((even? k) (* 2.0 (f (+ a (* k h)))))
              (else (* 4.0 (f (+ a (* k h)))))))
    (define (next k) (+ k 1))
    (* (/ h 3.0) (sum-iter term 0 next n)))


(define (integral-simpson-old f a b n)
    (define (sum-wrapper h)
        (define (factor k)
            (cond ((= k 0) 1)
                  ((= k n) 1)
                  ((even? k) 2)
                  (else 4)))
        (define (sum-body x k)
            (if (> x b) 
                0
                (+ (* (factor k) (f x)) (sum-body (+ x h) (+ k 1)))))
        (* (/ h 3.0) (sum-body a 0)))
    (sum-wrapper (/ (- b a) n)))
