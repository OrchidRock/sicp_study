;:
;:exercise 4.12
;:

(define (lookup-operate-env-variable var val env f-not-founded f-founded)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) ; not-founded
                (f-not-founded (enclosing-environment env)
                               (lambda (var val) (add-binding-to-frame! var val frame))))
            ((eq? var (car vars))
                (set-car! vals val)
                (car vals))
            (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
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
