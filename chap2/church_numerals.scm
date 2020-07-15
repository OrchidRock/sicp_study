;:
;: ercise 2.6
;: Church numerals
;;

(define zero (lambda (f) (lambda (x) x)))


(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add-2 n) (add-1 (add-1 n)))

;: (add-1 zero)
(define one (lambda (f) (lambda (x) (f x))))

;: (add-1 one)
(define two (lambda (f) (lambda (x) (f (f x)))))


(define (+ m n)
    (lambda (f) (lambda (x) ((m f) ((n f) x)))))
