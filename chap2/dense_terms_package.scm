
(load "data_directed_package.scm")

(define (install-dense-terms-package)
  
  
  (define (add-terms t1 t2)
    (cond ((null? t1) t2)
          ((null? t2) t1)
          (else (let ((c1 (car t1))
                      (c2 (car t2)))
                    (cons (add c1 c2) (add-terms (cdr t1) (cdr t2)))))))
  (define (mul-terms t1 t2)
    (if (null? t1)
        '()
        (add-terms (mul-term-by-all-terms (car t1) t2)
                   (mul-terms (cdr t1) t2))))
  (define (mul-term-by-all-terms t L)
    (if (null? L)
        '()
        (let ((t2 (car L)))
            (cons (mul t t2) (mul-term-by-all-terms t (cdr L))))))

  (define (negtive-terms t)
    (map (lambda(x) (negtive x)) t))

  (define (sub-terms t1 t2)
    (add-terms t1 (negtive-terms t2)))
  
  (define (dense-to-sparse terms)
    (define (iter t n result)
        (if (null? t)
            result
            (iter (cdr t) (+ n 1) (cons (list n (car t)) 
                                        result))))
    (iter terms 0 '()))

  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense) (lambda (x y) (tag (add-terms x y))))
  (put 'sub '(dense dense) (lambda (x y) (tag (sub-terms x y))))
  (put 'mul '(dense dense) (lambda (x y) (tag (mul-terms x y))))
  (put 'negtive '(dense) (lambda (x) (tag (negtive-terms x))))
  (put '=zero? '(dense) (lambda (x) (fold-right (lambda (m n) (and m n))
                                                true
                                                (map (lambda (y) (=zero? y))
                                                     x))))
  (put 'raise '(dense) (lambda (x) 
        ((get 'make 'sparse) (dense-to-sparse x))))

  (put 'type-index 'dense DENSE_INDEX)
  (put 'make 'dense (lambda (terms) (tag terms)))
'done)
