;:
;: interval arithmetic
;:
;: The maximum value and minimun value of arithmetic must be got upon bounds.

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

;: the reciprocal of interval
;: exercise 2.10
;:
(define (recip-interval x)
    (if (<= (* (lower-bound x) (upper-bound x)) 0)
        (error "the argument has an invalid value of recip-interval")
        (make-interval (/ 1.0 (upper-bound x))
                       (/ 1.0 (lower-bound x)))))

(define (div-interval x y)
    (mul-interval x (recip-interval y))) 
               

;: exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;: exercise 2.8
;: we define firstly the negative form of interval.
;:
(define (neg-interval x)
    (make-interval (- (upper-bound x))
                   (- (lower-bound x))))
(define (sub-interval x y)
    (add-interval x (neg-interval y)))


;: exercise 2.12
;: Define a constructor that takes a center and a percentage tolerance and produces the desired 
;: interval.
(define (make-center-percent c p)
    (let ((width (/ (* c p) 100)))
        (make-interval (- c width) (+ c width))))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

;:
;: add-interval: p = (c1p1 + c2p2)/(c1+c2), c = c1 + c2
;:              if p1=p2, then p = p1 = p2
;: neg-interval: p' = -p, c' = -c
;: sub-interval: p = (c1p1 + c2p2)/(c1-c2), c = c1 - c2
;: mul-interval: p = (p1+p2)/(1+p1p2%%), c = c1c2 + c1c2p1p2%%
;: recip-interval: p' = p, c' = 1/(c-cpp%%) 
;: div-interval: p = (p1+p2)/(1+p1p2%%), c = (c1+c1p1p2%%)/(c2-c2p2p2%%)
;:
(define (percent i)
    (* (/ (- (upper-bound i) (lower-bound i))
          (+ (upper-bound i) (lower-bound i))) 100))

;: exercise 2.13
;:
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))
(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one (add-interval (div-interval one r1)
                                        (div-interval one r2)))))

;: exercise 2.14
;: par1 will approach to par2 when p1 and p2 more and more small.

;: test
(define R1 (make-interval 6.12 7.48))
(define R2 (make-interval 4.465 4.935))
;(define R2 (make-interval -4.475 4.925))
(define RA (add-interval R1 R2))
(define RS (sub-interval R1 R2))
(define RM (mul-interval R1 R2))
(define RD (div-interval R1 R2))
(define RP2 (recip-interval (add-interval (recip-interval R1)
                                         (recip-interval R2))))
(define RP1 (par1 R1 R2))

(define R3 (make-center-percent 6.8 0.1))
(define R4 (make-center-percent 4.7 0.05))
(define R1R1 (div-interval R1 R1))
(define R2R2 (div-interval R2 R2))
(define R3R3 (div-interval R3 R3))
(define R4R4 (div-interval R4 R4))

;: exercise 2.15

(define (equal-interval? R1 R2)
    (and (= (upper-bound R1) (upper-bound R2))
         (= (lower-bound R1) (lower-bound R2))))

;: test: 1/(1/R1R1) = R1R1 ?
(equal-interval? (recip-interval (recip-interval (mul-interval R1 R2))) RM) ; #t
;:       1/(R2/R1) = R1/R2 ?
(equal-interval? (recip-interval (div-interval R2 R1)) RD); #t
;:       R1/(R1/R2) = R2 ?
(equal-interval? (div-interval R1 (div-interval R1 R2)) R2) ; #f

;: R1(R1 + R2) = R1R1 + R1R2 ? #t
(define T1 (mul-interval R1 (add-interval R1 R2)))
(define T2 (add-interval (mul-interval R1 R1) (mul-interval R1 R2)));
;: so that Eva Lu Ator's option is wrong, because of T1=T2

;: R1(R2-R1) = R1R2 - R1R1 ? #f
(define T3 (mul-interval R1 (sub-interval R2 R1)))
(define T4 (sub-interval (mul-interval R1 R2) (mul-interval R1 R1)))

;: R2(R2-R1) = R2R2 - R2R1 ? #f
(define T5 (mul-interval R2 (sub-interval R2 R1)))
(define T6 (sub-interval (mul-interval R2 R2) (mul-interval R2 R1)))

;: so if a variable be valued differently of two expressions, a expressions will
;: not be equal to another expressions.
;:

