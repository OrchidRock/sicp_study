;:
;:
;:


;: streams
;: cons-stream must be a special form, but I don't know how to define it.
(define (delay exp)
    (lambda () exp))
(define (force object)
    (object))

(define cons-stream
    (lambda (a b) (cons a (delay b)))) ; not working!!!

; MIT/GNU Scheme supports a higher-level abstraction called streams
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))


; execise 3.50
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream (apply proc (map stream-car argstreams))
               (apply stream-map proc (map stream-cdr argstreams)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))



;(define (stream-filter pred stream)
;  (cond ((stream-null? stream) the-empty-stream)
;        ((pred (stream-car stream))
;         (cons-stream (stream-car stream)
;                      (stream-filter pred
;                                     (stream-cdr stream))))
;        (else (stream-filter pred (stream-cdr stream)))))

;: some streams
