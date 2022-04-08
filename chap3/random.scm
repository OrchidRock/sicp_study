
;#lang scheme

(define RANDOM_INIT 5)

(define (rand-update x)
    (let ((a 27) (b 26) (m 127))
        (modulo (+ (* a x) b) m)))

;: exercise 3.6
;:(define (rand-new m)
;:  (let ((x RANDOM_INIT))
;:    (cond ((eq? m 'generate)
;:           (begin (set! x (rand-update x))
;:                  x))
;:          ((eq? m 'reset)
;:           (lambda (new-value) (set! x new-value)))
;:          (else (error "Please input <generate> or <reset>")))))

(define rand-new
    (let ((x RANDOM_INIT))
        (lambda (m)
            (cond ((eq? m 'generate)
                   (begin (set! x (rand-update x))
                          x))
                  ((eq? m 'reset)
                   (lambda (new-value)
                        (set! x new-value)
                        #t))
                  (else (error "Please input <generate> or <reset>"))))))

(define (rand)
    (rand-new 'generate))


(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))
