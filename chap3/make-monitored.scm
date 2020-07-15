;:
;: exercise 3.2
;:

(define (make-monitored f)
    (let ((counts 0))
        (lambda (m)
            (cond ((eq? m 'how-many-calls?) counts)
                  ((eq? m 'reset-count) (set! counts 0))
                  (else (begin (set! counts (+ counts 1))
                               (f m)))))))

;: Test
(define s (make-monitored sqrt))


