;:
;:
(load "type_index_dispatch.scm")

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((reduces-result (reduce n d)))
        (cons (car reduces-result) (cadr reduces-result))))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system

  (define (tag x)
     (let ((numer (numer x))
           (denom (denom x)))
        (if (equ? (gcd-common numer denom) denom)
            (div numer denom)
            (attach-tag 'rational x))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  ;: exercise 2.79
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equ? (denom x) (denom y))
                          (equ? (numer x) (numer y)))))
  (put '=zero? '(rational)
        (lambda (x) (=zero? (numer x))))

  (put 'make 'rational
       (lambda (n d) (attach-tag 'rational (make-rat n d))))

  ;: exercise 2.88
  (put 'negtive '(rational)
        (lambda (x) (tag (make-rat (negtive (numer x)) (denom x)))))
;: exercise 2.83
  (put 'raise '(rational)
     ;(lambda (x) ((get 'make-from-real-imag 'complex) (tag x) 0)))
     (lambda (x) ((get 'make 'real) (exact->inexact (/ (numer x) (denom x))))))

;: exercise 2.85
  (put 'project '(rational)
     (lambda (x) (round (/ (numer x) (denom x)))))
  (put 'drop '(rational)
     (lambda (x) ((get 'make 'integer) (inexact->exact (/ (numer x) (denom x))))))

  (put 'gcd '(rational rational) (lambda (x y) (tag (make-rat 1 1))))

  (put 'type-index 'rational RATIONAL_INDEX)

  'done)
