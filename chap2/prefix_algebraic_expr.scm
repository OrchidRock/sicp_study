;:
;: argebraic expressions presented in prefix form.
;:

(define (=number? expr num) (and (number? expr) (= expr num)))
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2) (and (variable? v1)
                                    (variable? v2)
                                    (eq? v1 v2)))

(define (sum? e) (and (pair? e) (eq? (car e) '+)))
(define (addend e) (cadr e))
(define (augend e) (caddr e))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2) (cond ((eq? a1 0) a2) ;: simplified
                               ((eq? a2 0) a1)
                               ((and (number? a1) (number? a2)) (+ a1 a2))
                               (else (list '+ a1 a2))))

(define (product? e) (and (pair? e) (eq? (car e) '*)))
(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))
(define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2) (cond ((eq? m1 0) 0) ;: simplified
                                   ((eq? m2 0) 0)
                                   ((eq? m1 1) m2)
                                   ((eq? m2 1) m1)
                                   ((and (number? m1) (number? m2)) (* m1 m2))
                                   (else (list '* m1 m2))))
;: exercise 2.57
;: Extend the differentiation program to handle sums and products of arbitrary numbers of
;: (two or more) terms.
(define (augend e) (if (null? (cdddr e))
                        (caddr e)
                        (cons '+ (cddr e))))

(define (multiplicand e) (if (null? (cdddr e))
                        (caddr e)
                        (cons '* (cddr e))))


;: exercise 2.56
(define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation b expon) (cond ((eq? expon 0) 1)
                                            ((eq? expon 1) b)
                                            ((eq? b 1) 1)
                                            ((and (number? b) (number? expon)) (exp b expon))
                                            (else (list '** b expon))))
