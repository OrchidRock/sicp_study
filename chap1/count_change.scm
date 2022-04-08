
(declare (usual-integrations))

;:
;: How many different ways can we make changes of $ 1.00.
;: The number of ways to change amount a using n kinds of coins equals
;:      a. the number of ways to change amount a using all but the first kind of coin, plus
;:      b. the number of ways to change amount a-d using all n kinds of coins, where d is
;:         the denomination of the first kind of coin.
;:
;: So, we have a simple solution as a recursive process.
;:
(define (count-change amount)
    (define (first-denomination kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
              ((= kinds-of-coins 2) 5)
              ((= kinds-of-coins 3) 10)
              ((= kinds-of-coins 4) 25)
              ((= kinds-of-coins 5) 50)))

    (define (cc amount kinds-of-coins)
        (cond ((= amount 0) 1)
              ((or (< amount 0) (= kinds-of-coins 0)) 0)
              (else (+ (cc amount (- kinds-of-coins 1)) ;; case a
                       (cc (- amount (first-denomination kinds-of-coins))
                           kinds-of-coins)))))          ;: case b

    (cc amount 5))
