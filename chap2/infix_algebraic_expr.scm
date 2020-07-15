;:
;: exercise 2.58
;: argebraic expressions presented in infix form.
;:

;: a)
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2) (and (variable? v1)
                                    (variable? v2)
                                    (eq? v1 v2)))

(define (sum? e) (and (pair? e) (eq? (cadr e) '+)))
(define (addend e) (car e))
(define (augend e) (caddr e))
(define (make-sum a1 a2) (cond ((eq? a1 0) a2) ;: simplified
                               ((eq? a2 0) a1)
                               ((and (number? a1) (number? a2)) (+ a1 a2))
                               (else (list a1 '+ a2))))

(define (product? e) (and (pair? e) (eq? (cadr e) '*)))
(define (multiplier e) (car e))
(define (multiplicand e) (caddr e))
(define (make-product m1 m2) (cond ((eq? m1 0) 0) ;: simplified
                                   ((eq? m2 0) 0)
                                   ((eq? m1 1) m2)
                                   ((eq? m2 1) m1)
                                   ((and (number? m1) (number? m2)) (* m1 m2))
                                   (else (list m1 '* m2))))
;: b)
(define (augend e) (if (null? (cdddr e))
                        (caddr e)
                        (cddr e)))

(define (multiplicand e) (if (null? (cdddr e))
                        (caddr e)
                        (cddr e)))

(define (exponentiation? e) (and (pair? e) (eq? (cadr e) '**)))
(define (base e) (car e))
(define (exponent e) (caddr e))
(define (make-exponentiation b expon) (cond ((eq? expon 0) 1)
                                            ((eq? expon 1) b)
                                            ((eq? b 1) 1)
                                            ((and (number? b) (number? expon)) (exp b expon))
                                            (else (list b '** expon))))
