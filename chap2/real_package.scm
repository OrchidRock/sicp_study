;:
;:

(load "data_directed_package.scm")
(load "type_index_dispatch.scm")

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
;: exercise 2.88
  (put 'negtive '(real)
        (lambda (x) (tag (- x))))

  ;: exercise 2.94
  ;(put 'gcd '(real real)
    ;(lambda (x y) (tag (gcd x y))))

  ;: exercise 2.97
  ;(put 'reduce '(real real)
   ; (lambda (x y) (let ((g (gcd x y)))
    ;                (list (/ x g) (/ y g)))))
;: exercise 2.79
;: exercise 2.80
  (put 'equ? '(real real)
        (lambda (x y) (= x y)))
  (put '=zero? '(real)
        (lambda (x) (= x 0)))

;: exercise 2.83
  (put 'raise '(real)
    ;(lambda (x) ((get 'make 'rational) x 1)))
    (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
;: exercise 2.85
  (put 'project '(real)
    (lambda (x) (inexact->exact (round x))))

  (put 'type-index 'real REAL_INDEX)

  'done)
