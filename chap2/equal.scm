;:
;: 
;:

(define (equal? x y)
    (cond ((and (pair? x) (pair? y)) (and (equal? (car x) (car y))
                                          (equal? (cdr x) (cdr y))))
          ((and (not (pair? x)) (not (pair? y))) (eq? x y))
          (else false)))

;: Test
(define x (list 'a 'b 'c))
(define x2 '(a b c))
(define x3 '(this is (a b) list))
(define x4 '(this is (a c) list))
(define x5 '(this is (a c) list))
(define x6 '((this is) (a c) list))
