;:
;: sum and product are both special cases of a still more general notion called 
;: accumulate that combains a collection of terms, using some general accumulation
;: function.
;:


;: exercise 1.32 a)
;: recursive version
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner 
                              null-value 
                              term 
                              (next a)
                              next
                              b))))
;: exercise 1.32 b)
;: iterative version
(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))


;: re-define sum which be called within sum.scm
(define (sum term a next b)
    (accumulate + 0 term a next b))

(define (sum-iter term a next b)
    (accumulate-iter + 0 term a next b))

;: re-define product which be called within product.scm
(define (product term a next b)
    (accumulate * 1 term a next b))

(define (product-iter term a next b)
    (accumulate-iter * 1 term a next b))


;: application: 

;: sum.scm
;: product.scm 

(define (sum2-filter term a next b)
  (define (my-filter x) (or (< x b) (= x b)))
  (filtered-accumulate my-filter + 0 term a next ))




(define (pi-sum2-filter a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (* 8 (sum2-filter pi-term a pi-next b)))



