;:
;: sine.scm
;: Note: when x --> 0,the limition of (sin x)/x ---> 1.
;: sin function has the following identical equation:
;: sin x= 3 * sin(x/3) - 4 * (sin(x/3))^3

(declare (usual-integrations))

(define (sine angle)
    (define (cude x) (* x x x ))
    (define (p x) (- (* 3 x) (* 4 (cude x))))
    (if (<= (abs angle) 0.1)
        angle
        (p (sine (/ angle 3.0)))))

;: sin(12.15) = -0.4044438228491401
;: sine(12.15) = -0.39980345741334 
;: p will be called 
