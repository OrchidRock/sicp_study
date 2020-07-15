
;:
;:

(load "data_directed_package.scm")
(load "rectangular_complex_package.scm")
(load "polar_complex_package.scm")
(load "scheme_number_package.scm")
(load "rational_package.scm")
(load "complex_package.scm")
(load "type_index_dispatch.scm")
(load "sparse_terms_package.scm")
(load "dense_terms_package.scm")
(load "polynomial_package.scm")

(define (apply-generic op . args)
    (display op)
    (display " ")
    (display args)
    (newline)

    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (cond ((= (length args) 2)
                        (let ((type1-index (type-index (car type-tags)))
                              (type2-index (type-index (cadr type-tags)))
                              (a1 (car args))
                              (a2 (cadr args)))

                              ;(display type1-index)
                              ;(display type2-index)
                              ;(display a1)
                              ;(display a2)
                              (cond ((and (= type1-index COMPLEX_INDEX)
                                          (= type2-index COMPLEX_INDEX))
                                        (error "No method for these type -- APPLY-GENERIC" (list op type-tags)))
                                    ((and (= type1-index POLYNOMIAL_INDEX)
                                          (= type2-index POLYNOMIAL_INDEX))
                                        (error "No method for these type -- APPLY-GENERIC" (list op type-tags)))
                                    ((= type1-index type2-index) (apply-generic op (raise a1) (raise a2)))
                                    ((= type2-index POLYNOMIAL_INDEX) (apply-generic op
                                                                            (raise-to-poly a1 
                                                                              (variable (contents a2)))
                                                                            a2))
                                    ((= type1-index POLYNOMIAL_INDEX) (apply-generic op
                                                                            a1
                                                                            (raise-to-poly 
                                                                              a2 
                                                                              (variable (contents a1)))))
                                    ((> type2-index type1-index) (apply-generic op (raise-n a1 (- type2-index type1-index)) a2))
                                    (else (apply-generic op a1 (raise-n a2 (- type1-index type2-index)))))))
                      ((= (length args) 1) 
                        (let ((t (type-index (car type-tags)))
                              (a (car args)))
                            (if (or (= t COMPLEX_INDEX)
                                     (= t SPARSE_INDEX))
                                (error "No method for these type -- APPLY-GENERIC" (list op type-tags))
                                (apply-generic op (raise a)))))
                      (else (error "No method for these type -- APPLY-GENERIC" (list op type-tags))))))))

;: generic operator
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;: exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))
;: exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;: exercise 2.83
(define (raise x) (apply-generic 'raise x))

(define (raise-n arg n)
    (if (= n 0) 
        arg
        (raise-n (raise arg) (- n 1))))

;: exercise 2.88
(define (negtive x) (apply-generic 'negtive x))

;: exercise 2.93
;: exercise 2.94
(define (gcd-common x y) (apply-generic 'gcd x y))

;: exercise 2.97
(define (reduce x y) (apply-generic 'reduce x y))

;: install packages
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-rational-package)
(install-scheme-number-package)
(install-dense-terms-package)
(install-sparse-terms-package)
(install-polynomial-package)


;(define (make-from-real-imag x y)
;   ((get 'make-from-real-imag 'rectangular) x y))
;(define (make-from-mag-ang r a)
;    ((get 'make-from-mag-ang 'polar) r a))

(define (make-from-scheme-number n)
    ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial-from-sparse var terms)
    ((get 'make-from-sparse 'polynomial) var terms))
(define (make-polynomial-from-dense var terms)
    ((get 'make-from-dense 'polynomial) var terms))

(define (raise-to-poly x y)
    ((get 'raise-to-poly 'polynomial) x y))
(define (variable x)
    ((get 'variable 'polynomial) x))

(define (type-index x) 
    (get 'type-index x))
;:
;: Test
(define x1 (make-rational 0 1))
(define x2 (make-from-scheme-number 0))
(define x3 (make-from-scheme-number 5))
(define y3 (make-from-scheme-number 6))
(define x4 (make-rational 1 2))
(define y4 (make-rational 2 3))
(define x5 (make-complex-from-mag-ang 1.5 2.0))
(define y5 (make-complex-from-mag-ang 0.5 1.0))
(define y6 (make-complex-from-mag-ang 0.5 1.0))
(define y7 (make-complex-from-mag-ang 0.0 1.0))
(define y8 (make-complex-from-real-imag x4 y4))
(define y9 (make-complex-from-real-imag x3 y3))

;: 3x^2 + (2+3i)x + 7
(define y10 (make-polynomial-from-sparse 'x (list (list 2 3)
                                      (list 1 (make-polynomial-from-sparse 'i (list (list 1 3)
                                                                        (list 0 2))))
                                       (list 0 7))))
;: x^4 + (2/3)*x^2 + (5+3i)
(define y11 (make-polynomial-from-sparse 'x (list (list 4 1)
                                      (list 2 (make-rational 2 3))
                                      (list 0 (make-polynomial-from-sparse 'i (list (list 1 3)
                                                                        (list 0 5))))
                                                                        )))
(define y12 (make-polynomial-from-sparse 'x (list (list 2 (make-polynomial-from-sparse 'y (list (list 1 1)
                                                                        (list 0 1))))
                                      (list 1 (make-polynomial-from-sparse 'y (list (list 2 1)
                                                                        (list 0 1))))
                                      (list 0 (make-polynomial-from-sparse 'y (list (list 1 1)
                                                                        (list 0 -1)))))))
(define y13 (make-polynomial-from-sparse 'x (list (list 1 (make-polynomial-from-sparse 'y (list (list 1 1)
                                                                        (list 0 -2))))
                                      (list 0 (make-polynomial-from-sparse 'y (list (list 3 1)
                                                                        (list 0 7)))))))
(define y14 (make-polynomial-from-sparse 'x (list (list 2 1)
                                      (list 1 (make-polynomial-from-sparse 'y (list (list 1 1)
                                                                        (list 0 1))))
                                      (list 0 5))))
(define y15 (make-polynomial-from-sparse 'x (list (list 2 1)
                                      (list 1 2)
                                      (list 0 1))))

(define y16 (make-polynomial-from-sparse 'x (list (list 3 0)
                                      (list 2 0)
                                      (list 1 (make-polynomial-from-sparse 'y (list (list 1 0)
                                                                        (list 0 0))))
                                      (list 0 0))))

(define y17 (make-polynomial-from-dense 'x (list 7
                                                (make-polynomial-from-dense 'i (list 2 3))
                                                3)))
(define y18 (make-polynomial-from-dense 'x (list (make-polynomial-from-dense 'i (list 5 3))
                                                 0
                                                 (make-rational 2 3)
                                                 0
                                                 1)))

(define y19 (make-polynomial-from-sparse 'x (list (list 5 1)
                                                  (list 0 -1))))
(define y20 (make-polynomial-from-sparse 'x (list (list 2 1)
                                                  (list 0 -1))))

(define y21 (make-polynomial-from-sparse 'x (list (list 1 1)
                                                  (list 0 1))))
(define y22 (make-polynomial-from-sparse 'x (list (list 3 1)
                                                  (list 0 -1))))
(define y23 (make-polynomial-from-sparse 'x (list (list 1 1))))
(define y24 (make-polynomial-from-sparse 'x (list (list 2 1)
                                                  (list 0 -1))))

(define r1 (make-rational y21 y22))
(define r2 (make-rational y23 y24))

;: exercise 2.95
(define p1 (make-polynomial-from-sparse 'x (list (list 2 1)
                                                 (list 1 -2)
                                                 (list 0 1))))
(define p2 (make-polynomial-from-sparse 'x (list (list 2 11)
                                                 (list 0 7))))
(define p3 (make-polynomial-from-sparse 'x (list (list 1 13)
                                                 (list 0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
