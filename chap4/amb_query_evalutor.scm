;:
;:
;:

(load "amb_evaluator.scm")
(load "query_evalutor.scm")


(define (get-input)
    (if (null? pre-eval-procedures)
        (let ((input (read)))
            (if (eq? input 'try-again)
                'try-again
                (amb-query-syntax-process (query-syntax-process input) '())))
        (let ((first-proc (car pre-eval-procedures)))
            (set! pre-eval-procedures (cdr pre-eval-procedures))
            first-proc)))

(define (amb-query-syntax-process query-exp frame)
        (cond ((assertion-to-be-added? query-exp)
                (add-rule-or-assertion! (add-assertion-body query-exp))
                (newline)
                (display "Assertion added to data base.")
                (newline)
                (driver-loop))
              (else
                (let ((qproc (get (type query-exp) 'qeval)))
                    (if qproc
                        (qproc (contents query-exp) frame)
                        (simple-query-handler query-exp frame))))))

(define (unbound-var-handler v f)
    (lambda (v f) (contract-question-mark v)))

;: simple query
(define (simple-query-handler pattern frame)
    (print-procedure-message "simple-query-handler: " pattern frame)
    (let ((instantiated-pattern (instantiate pattern frame unbound-var-handler)))
        (let ((assertions (fetch-assertions instantiated-pattern frame)))
            (make-let 
                (list (list 'assertion (make-amb assertions))) 
                (list (list 'require 
                             (list 'pattern-match-amb
                                    (list 'quote (query-syntax-process instantiated-pattern))
                                    'assertion
                                    (list 'quote '())))
                      'assertion)))))

(define (conjoin operands frame)
    (let ((instantiated-pattern (instantiate (car operands) frame unbound-var-handler)))
        (let ((assertions (fetch-assertions instantiated-pattern frame)))
            (make-let 
                (list (list 'assertion1 (make-amb assertions)))
                (make-let
                    (list (list 'frame1 
                                (list 'pattern-match-amb
                                      (list 'quote 
                                            (query-syntax-process instantiated-pattern)))))
                    (list 'require 'frame1))))))

    

(define (disjoin operands frame)())

;: override
;(define (amb-choices exp) (cadr exp))

(define (get-compound-symbol name count)
    (string->symbol (string-append (symbol->string name) (number->string count))))

(define (make-amb assertions-stream)
    (define (get-amb-choices stream)
        (if (stream-null? stream)
            '()
            (cons (list 'quote (query-syntax-process (stream-car stream))) 
                  (get-amb-choices (stream-cdr stream)))))
    (cons 'amb (get-amb-choices assertions-stream)))

(define (pattern-match-amb pattern assertion frame)
    (let ((r (pattern-match pattern assertion frame)))
        (if (eq? r 'failed)
            false
            r)))

;: compound query
;(and (job ?x ?y)
;     (salary ?z ?x))
;=>
;(let ((assertion1 (amb (fetch-assertions (.1.))))
;    (let ((frame1 (pattern-match (.1.) assertion1)))
;        (require frame1)
;        (let ((a2 (instantiate (.2.) frame1))
;              (assertion2 (amb (fetch-assertions (.2.)))))
;            (require (pattern-match a2 assertion2)))))


(define input-prompt ";;; Amb-query input:")
(define output-prompt ";;; Amb-query output:")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
        (list 'prime? prime?) ;: load prime.scm
        (list 'display display)
        (list 'newline newline)
        (list 'pattern-match-amb pattern-match-amb)
        (list 'instantiate instantiate)
;;      more primitives
        ))

(define the-global-environment (setup-environment))
(initialize-data-base microshaft-data-base)
(driver-loop)
