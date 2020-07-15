;:
;:exercise 4.12
;: 

(define (lookup-operate-env-variable var val env f-not-founded f-founded)
    (define (env-loop env)
        (define (scan f)
            (cond ((null? f) ; not-founded
                (f-not-founded (enclosing-environment env)
                               (lambda (var val) (add-binding-to-frame! var val frame))))
            ((eq? var (caar f))
                (f-founded (car f)))
            (else (scan (cdr f)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
                (scan frame))))
    (env-loop env))

(define (set-variable-value! var val env)
    (lookup-operate-env-variable var val env 
                                    (lambda (e f) (set-variable-value! var val e))
                                    (lambda (pair) (set-cdr! pair val))))

(define (define-variable! var val env)
    (lookup-operate-env-variable var val env
                                    (lambda (e f) (f var val))
                                    (lambda (pair) (set-cdr! pair val))))

(define (lookup-variable-value var env)
    (lookup-operate-env-variable var 0 env
                                    (lambda (e f) (lookup-variable-value var e))
                                    (lambda (pair) (cdr pair))))
