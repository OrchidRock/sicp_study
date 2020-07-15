;:
;:
(load "conventional_interfaces.scm")
(define (remove-item item sequence)
    (filter (lambda (x) (not (= x item))) sequence))
