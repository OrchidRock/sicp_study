;:
;: Squre Roots by Newton's method.
;:
;: f(Xn) = (f(Xn-1) + (a/f(Xn-1))) / 2
;:
;: exercise 1.6
;: the interpreter is using applicative-order so that all parameters of new-if
;: will be extened before extent itself. Dead loop will be happended!

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;: ex1.7: When finding the square roots of very small numbers
;; (sqrt 0.00000000003242)
;: Value: .03125000034547563 (wrong value)

;:(define (good-enough? guess x)
;:  (< (abs (- (square guess) x)) 0.001))
(define (good-enough? guess old-guess)
    (< (abs (- old-guess guess)) 0.001))

(define (sqrt-iter guess x)
   (if (good-enough? guess x)
;: (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt-iter-improve guess old-guess x)
   (if (good-enough? guess old-guess)
      guess
      (sqrt-iter-improve (improve guess x)
                        guess
                        x)))

(define (sqrt x)
;:    (sqrt-iter 1.0 x))
      (sqrt-iter-improve 1.0 0.0 x))
