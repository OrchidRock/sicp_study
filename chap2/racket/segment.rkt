#lang racket

(provide make-segment start-segment end-segment)

(define (make-segment vs ve) (cons vs ve))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
