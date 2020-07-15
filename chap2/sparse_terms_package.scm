;:
;: 
(load "data_directed_package.scm")
(define (install-sparse-terms-package)
    
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    ;(display "add-terms: ")
    ;(display L1)
    ;(display "   ")
    ;(display L2)
    ;(newline)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
 
 (define (div-terms L1 L2)
    (display "div-terms: ")
    (display L1)
    (display " ")
    (display L2)
    (newline)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                    ;(display "new-c: ")
                    ;(display new-c)
                    ;(display "  new-o:")
                    ;(display new-o)
                    ;(newline)
                    (let ((new-quotient (adjoin-term (make-term new-o new-c)
                                                     (the-empty-termlist))))
                        (let ((rest-of-result (div-terms (sub-terms L1 
                                                                (mul-terms L2
                                                                           new-quotient))
                                                      L2)))
                            (list (add-terms new-quotient (car rest-of-result))
                              (cadr rest-of-result)))))))))

 (define (negtive-terms ts)
    (if (empty-termlist? ts)
        (the-empty-termlist)
        (let ((t2 (first-term ts)))
            (adjoin-term
                (make-term (order t2)
                           (negtive (coeff t2)))
                (negtive-terms (rest-terms ts))))))
  (define (sub-terms L1 L2)
    (add-terms L1 (negtive-terms L2)))

  ;: exercise 2.94
  (define (gcd-terms a b)
    (display "gcd-terms: ")
    (display a)
    (display " ")
    (display b)
    (newline)
    (cond ((empty-termlist? a) (the-empty-termlist))
          ((empty-termlist? b) 
            (let ((gcd-a (apply gcd (map coeff a))))
                (map (lambda (x) (make-term (order x) (/ (coeff x) gcd-a))) a)))
          (else  (let ((t1 (first-term a))
                       (t2 (first-term b)))
                    (if (< (order t1) (order t2))
                        (gcd-terms b a)
                        (gcd-terms b (pseudoremainder-terms a b)))))))
  
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  
  ;: exercise 2.96
  (define (pseudoremainder-terms a b)
    (let ((t1 (first-term a))
          (t2 (first-term b)))
        (let ((c1 (coeff t1))
              (c2 (coeff t2))
              (o1 (order t1))
              (o2 (order t2)))
            (if (and (integer? c1) (integer? c2))
                (let ((f (expt c2 (+ 1 (- o1 o2)))))
                    (cadr (div-terms (mul-terms a (list (make-term 0 f))) b)))
                (remainder-terms a b)))))
  
  ;: exercise 2.97
  (define (reduce-terms a b)
    (let ((g-term (gcd-terms a b)))
        (list (car (div-terms a g-term)) 
              (car (div-terms b g-term)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse) 
       (lambda (p1 p2) (tag (add-terms p1 p2))))
  (put 'mul '(sparse sparse) 
       (lambda (p1 p2) (tag (mul-terms p1 p2))))
  ;: exercise 2.91
  (put 'div '(sparse sparse) 
       (lambda (p1 p2) (map tag (div-terms p1 p2))))
  (put 'negtive '(sparse) (lambda (p) (tag (negtive-terms p))))
  (put 'sub '(sparse sparse)
    (lambda (p1 p2) (tag (sub-terms p1 p2))))
  (put '=zero? '(sparse) (lambda (x) (fold-right (lambda (m n) (and m n))
                                            true
                                            (map (lambda (y) (=zero? (coeff y)))
                                                 x))))
  ;: exercise 2.94
  (put 'gcd '(sparse sparse) 
    (lambda (x y) (tag (gcd-terms x y))))
  ;: exercise 2.97
  (put 'reduce '(sparse sparse)
    (lambda (x y) (map tag (reduce-terms x y))))

  (put 'type-index 'sparse SPARSE_INDEX)
  (put 'make 'sparse (lambda (terms) (tag terms)))
 'done)
