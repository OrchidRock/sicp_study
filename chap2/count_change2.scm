;:
;: exercise 2.19
;: It would be nicer to be able to supply a list of coins to be used for making change.
;:

(define us-coins (list 25 50 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (define (except-first-denomination l) (cdr l))
    (define (no-more? l) (null? l))
    (define (first-denomination l) (car l))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount (except-first-denomination coin-values))
                   (cc (- amount (first-denomination coin-values)) coin-values)))))

;: test
(define T1 (cc 100 us-coins))
(define T2 (cc 50 uk-coins))
