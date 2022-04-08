;:
;: exercise 3.16
;: exercise 3.17
;:



(define (count-pairs pairs)
    (define (iter x record)
        (cond ((null? x) record)
              ((not (pair? x)) record)
              ((memq x record) record)
              (else
                (iter (car x)
                      (iter (cdr x)
                            (cons x record))))))
    (length (iter pairs '())))

;: Test
(define x1 (list 'x))
(define t1 (list 'a 'b 'c))
(define t2 (cons x1 x1))
(define t3 (cons t2 t2))

(define t4 (list t2))
