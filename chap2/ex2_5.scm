;:
;: An alternative procedural representation of pairs.
;: 

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car z)
    (cond ((<= z 0) (error "the argument has a invalid value"))
          ((even? z) (+ 1 (car (/ z 2))))
          (else 0)))

(define (cdr z)
    (cond ((<= z 0) (error "the argument has a invalid value"))
          ((= (remainder z 3) 0) (+ 1 (cdr (/ z 3))))
          (else 0)))
