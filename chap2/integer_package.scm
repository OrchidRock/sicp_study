;:
;:

(load "data_directed_package.scm")
(load "type_index_dispatch.scm")

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y)
            (if (= (remainder x y) 0)
                (tag (/ x y))
                ((get 'make 'rational) x y))))
  (put 'make 'integer
       (lambda (x) (tag x)))
;: exercise 2.88
  (put 'negtive '(integer)
        (lambda (x) (tag (- x))))

  ;: exercise 2.94
  (put 'gcd '(integer integer)
    (lambda (x y) (tag (gcd x y))))

  ;: exercise 2.97
  (put 'reduce '(integer integer)
    (lambda (x y) (let ((g (gcd x y)))
                    (list (/ x g) (/ y g)))))
;: exercise 2.79
;: exercise 2.80
  (put 'equ? '(integer integer)
        (lambda (x y) (= x y)))
  (put '=zero? '(integer)
        (lambda (x) (= x 0)))

;: exercise 2.83
  (put 'raise '(integer)
    (lambda (x) ((get 'make 'rational) x 1)))

  ;(put 'raise '(integer)
    ;(lambda (x) ((get 'make 'polynomial) 'i (list (list 0 x)))))

  (put 'type-index 'integer INTEGER_INDEX)

  'done)
