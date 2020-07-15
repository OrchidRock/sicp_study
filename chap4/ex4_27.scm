;:
;: exercise 4.27
;:
(define count 0)
(define (id x)
    (set! count (+ count 1))
    x)

;: exercise 4.29
(define (square x) (* x x))

(square (id 10))

;: When the evaluator memoized, the count be valued 1;
;: but when it dose not, the count be valued 2.
;: So we has noticed that Lazy evaluation combined with memoization
;: always constructe some comfusing problems.
