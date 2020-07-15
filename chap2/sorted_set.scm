;:
;:
;:


(define (element-of-set? x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;: complexity: ThThe(n)
(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1)
                                                        (cdr set2))))
                  ((< x1 x2) (intersection-set (cdr set1) set2))
                  ((< x2 x1) (intersection-set set1 (cdr set2)))))))

;: exercise 2.61
(define (adjoin-set x set)
    (cond ((null? set) '())
          ((< x (car set)) (cons x set))
          ((= x (car set)) set)
          ((null? (cdr set)) (cons (car set) (cons x (cdr set))))
          (else (cons (car set) (adjoin-set x (cdr set))))))

;: exercise 2.62
(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
                (let ((x1 (car set1)) (x2 (car set2)))
                    (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                          ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                          ((< x2 x1) (cons x2 (union-set set1 (cdr set2)))))))))
;Test
(define x1 (list 1 2 3 4 5))
(define x2 (list 3 4 5 6 7 8))
(define x3 (list 3 5 6 7 9 11))
