;:
;: exercise 3.4
;:

(define (make-account balance password)
  (let ((incorrect-access-counts 0))
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (call-the-cops)
        (error ("Wraning!!!! Calling the cops")))
    (define (dispatch pw m)
        (cond ((>= incorrect-access-counts 7) call-the-cops)
              ((not (eq? pw password)) (begin (set! incorrect-access-counts (+ incorrect-access-counts))
                                              (display "Incorrect password")))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
    dispatch))

;: Test
(define acc (make-account 100 'howwwppp))
((acc 'sa 'withdraw) 10)
((acc 'sa 'withdraw) 10)
((acc 'sa 'withdraw) 10)
((acc 'sa 'withdraw) 10)
((acc 'sa 'withdraw) 10)
((acc 'sa 'withdraw) 10)
