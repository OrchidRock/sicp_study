
;; exercise 3.74


(define (sign-change-detector x y)
    (if (>= x 0)
        (if (>= y 0) 0 -1)
        (if (< y 0) 0 1)))
(define sense-data (cons-stream 1
                   (cons-stream 2
                   (cons-stream 1.5
                   (cons-stream 1
                   (cons-stream 0.5
                   (cons-stream -0.1
                   (cons-stream -2
                   (cons-stream -3
                   (cons-stream -2
                   (cons-stream -0.5
                   (cons-stream 0.2
                   (cons-stream 3
                   (cons-stream 4 '()
                    ))))))))))))))

(define zero-crossings
    (stream-map sign-change-detector sense-data
                                     (cons-stream 0 sense-data)))
