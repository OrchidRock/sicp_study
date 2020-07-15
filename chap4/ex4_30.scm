;:
;: exercise 4.30
;:

; a)
(define (for-each proc items)
    (if (null? items)
        'done
        (begin (proc (car items))
               (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;: b)
;: eval: e => (trunk (set! x (cons x '(2))) <env>)
;:     it will not be valued if we use the original eval-sequence.
;:
(define (p1 x)
    (set! x (cons x '(2)))
    x)
(define (p2 x)
    (define (p e)
        e
        x)
    (p (set! x (cons x '(2)))))
