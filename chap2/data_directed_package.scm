;:
;: Data-Directed Programming and Additivity
;:

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;: exercise 2.78
;: 
(define (attach-tag type-tag contents)
    (if (eq? type-tag 'scheme-number)
        contents
        (cons type-tag contents)))

(define (type-tag datum)
    (cond ((pair? datum) (car datum))
          ((number? datum) 'scheme-number)
          (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
    (cond ((pair? datum) (cdr datum))
          ((number? datum) datum)
          (else (error "Bad tagged datum -- CONTENTS" datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

