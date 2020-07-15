;:
;:

(load "data_directed_package.scm")
(load "type_index_dispatch.scm")

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
;: exercise 2.88
  (put 'negtive '(scheme-number)
        (lambda (x) (tag (- x))))
  
  ;: exercise 2.94
  (put 'gcd '(scheme-number scheme-number)
    (lambda (x y) (tag (gcd x y))))
  
  ;: exercise 2.97
  (put 'reduce '(scheme-number scheme-number)
    (lambda (x y) (let ((g (gcd x y)))
                    (list (/ x g) (/ y g)))))
;: exercise 2.79 
;: exercise 2.80
  (put 'equ? '(scheme-number scheme-number)
        (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
        (lambda (x) (= x 0)))
  
;: exercise 2.83
  (put 'raise '(scheme-number) 
    (lambda (x) ((get 'make 'rational) x 1)))
  ;(put 'raise '(scheme-number) 
    ;(lambda (x) ((get 'make 'polynomial) 'i (list (list 0 x)))))
  (put 'type-index 'scheme-number SCHEME_NUMBER_INDEX)

  'done)
