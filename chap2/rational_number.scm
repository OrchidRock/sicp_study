;: 
;: Arithmetic Operations For Rational Numbers
;: 

;: Data abstraction
;: selectors and constructors

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;; reducing to lowest terms in constructor
(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

;: exercise 2.1
;: Define a better version of make-rat that handles both positive and negative arguments.
(define (make-rat n d)
    (let ((g (gcd n d)))
        (if (< d 0)
            (cons (/ (- n) g)
                  (/ (- d) g))
            (cons (/ n g)
                  (/ d g)))))


;: Arithmetic Operations
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

;: display
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))
