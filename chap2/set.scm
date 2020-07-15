;:
;:
;:


(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
            (cons (car set1)
                  (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

;: exercise 2.59
(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((not (element-of-set? (car set1) set2)) 
            (cons (car set1) (union-set (cdr set1) set2)))
          (else (union-set (cdr set1) set2))))
;: Test
(define x1 (list 1 2 3 4))
(define x2 '(a b cdsdf fd))
(define T1 (element-of-set? 6 x1))
(define T2 (element-of-set? 3 x1))
(define T3 (element-of-set? 'a x2))
(define T4 (adjoin-set 'c x2))
(define T5 (adjoin-set 'b x2))
(define T6 (intersection-set x1 x2))
(define T7 (intersection-set T4 T5))
