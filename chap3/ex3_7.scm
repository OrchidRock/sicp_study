;:
;:
;:

(load "ex3_3.scm")

(define (make-joint acount password new-password)
    (lambda (pw m)
        (cond ((eq? pw new-password)
                (acount password m))
                (else (error "Incorrect password")))))

;:
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
