;:
;: exercise 2.73
;:

(load "prefix_algebraic_expr.scm")
(load "data_directed_package.scm")

(define (deriv expr var)
    (cond ((number? expr) 0)
          ((variable? expr) (if (same-variable? expr var) 1 0))
          (else ((get 'deriv (operator expr)) (operands expr) var ))))

(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

;: sum form
(define (install-sum-package)
    (define (deriv-sum expr_operands var)
            (make-sum (deriv (car expr_operands) var)
                      (deriv (cadr expr_operands) var)))
    ;:
    (put 'deriv '+ deriv-sum))

;: product form
(define (install-product-package)
    (define (deriv-product expr_operands var)
        (make-sum (make-product (car expr_operands)
                                (deriv (cadr expr_operands) var))
                  (make-product (cadr expr_operands)
                                (deriv (car expr_operands) var))))
    (put 'deriv '* deriv-product))

;: Test

(install-sum-package)
(install-product-package)


(define x1 '(+ x 3))
(define x2 '(* x y))


(define x3 '(* (* x y) (+ x 3)))
