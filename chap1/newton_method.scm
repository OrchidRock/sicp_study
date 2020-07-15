;: Newton's method be used to find roots of g(x)=0.

(declare (usual-integrations))

;: This algorithm based on fixed-point algorithm.
(load "fixed_point.scm")

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
 (lambda (x) (average x ( f x))))

(define dx 0.00001)

;:
;: deriv's return value is a procedure.
;: g(x) => Dg(x)
;:
(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g) (lambda (x) (- x (/ ( g x) ((deriv g) x)))))

(define (newton-method g guess) (fixed-point (newton-transform g) guess))

(define (sqrt x) (newton-method (lambda (y) (- (* y y) x)) 1.0))

;: re-definne sqrt which based on fixed-point-of-transform
(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (- (square y) x))
                              newton-transform
                              1.0))
