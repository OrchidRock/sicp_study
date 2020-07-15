;:
;: exercise 2.29
;:

(define (make-mobile left right)
    (list left right))

(define (make-branch len structure)
    (list len structure))

;: a)
(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))


;: b)
(define (total-weight m)
    ;(newline)
    ;(display m)
    (cond ((null? m) 0)
          ((pair? (left-branch m)) (+ (total-weight (left-branch m))
                                      (total-weight (right-branch m))))
          ((not (pair? (branch-structure m))) (branch-structure m))
          (else (total-weight (branch-structure m)))))

;: iterative version
(define (total-weight-iter m)
    (define (iter m result)
        (newline)
        (display m)
        (display " ")
        (display result)
        
        (cond ((null? m) result)
              ((pair? (left-branch m)) (iter (right-branch m) 
                                                         (+ result 
                                                            (iter (left-branch m) result))))
              ((not (pair? (branch-structure m))) (+ result 
                                                     (branch-structure m)))
              (else (iter (branch-structure m) result))))
    (iter m 0))

;: c)
(define (balanced? m)
    (define (iter m)
        (newline)
        (display m)
        (cond ((pair? (left-branch m)) 
                     (let ((ll (branch-length (left-branch m))) ; left-branch length
                           (lw (iter (left-branch m))))         ; left-branch weight
                          (display " ll=")
                          (display ll)
                          (display " lw=")
                          (display lw)

                          (if (< lw 0)
                              -1 ; left-branch not balanced
                              (let ((rl (branch-length (right-branch m)));right-branch length
                                    (rw (iter (right-branch m))))        ;right-branch weight
                                   (display " rl=")
                                   (display rl)
                                   (display " rw=")
                                   (display rw)

                                   (cond ((< rw 0) -1) ; right-branch not balanced
                                         ((not (= (* ll lw) (* rl rw))) -1) ; m not balanced
                                         (else (+ lw rw))))))) ; m balanced
              ((not (pair? (branch-structure m))) (branch-structure m))
              (else (iter (branch-structure m)))))
    (>= (iter m) 0))

;: Test
(define x (list (list 2 (list (list 3 4) (list 1 5))) (list 6 8)))
(define x2 (list (list 2 (list (list 10 4) (list 8 5))) (list 6 3)))

;: d)
(define (branch-structure b) (cdr b))
(define (right-branch m) (cdr m))
(define x3 (cons (cons 2 (cons (cons 10 4) (cons 8 5))) (cons 6 3)))
