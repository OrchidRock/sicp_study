;; exercise 3.80

(load "solve.scm")
(define (RLC R L C dt)
    (lambda (Vc0 iL0)
        (define Vc (integral (delay dVc) Vc0 dt))
        (define iL (integral (delay diL) iL0 dt))
        (define dVc (scale-stream iL (/ -1.0 C)))
        (define diL (add-streams (scale-stream iL (/ (* -1 R) L))
                                 (scale-stream Vc (/ 1.0 L))))
        (cons Vc iL)))

(define RLC1 (RLC 1 1 0.2 0.1))

(define test (RLC1 10 0))
