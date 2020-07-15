;:
;: LAZY EVALUTOR
;:

(load "metacircular_evaluator.scm")

(define (eval exp env)
  (display "eval: ")
  (print-exp exp)
  ;(display "pair?: " (application? exp))
  (newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (quoted->lazy-list exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        
        ;: exercise 4.4
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        
        ;; show how to implement 'and and 'or as derived expressions.
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or-if exp) env))
        
        ;: exercise 4.6
        ((let? exp) (eval (let->combination exp) env))
        
        ;; exercise 4.7
        ((let*? exp) (eval (let*->nested-lets exp) env))
        
        ;: exercise 4.9
        ((while? exp) (eval (while->combination exp) env))
        
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp) 
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments env)  
    (cond ((primitive-procedure? procedure)
                (apply-primitive-procedure procedure 
                                           (list-of-arg-values arguments env)))
          ((compound-procedure? procedure)
                (display "apply-compound-procedure :")
                (display (procedure-parameters procedure))
                (display " ")
                (display (procedure-body procedure))
                (display "  ")
                (user-print-objects arguments)
                ;(display arguments)
                (newline)
                
                (eval-sequence
                    (procedure-body procedure)
                    (extend-environment
                        (procedure-parameters procedure)
                        (list-of-delayed-args arguments env)
                        (procedure-environment procedure))))
          (else (error "Unknown procedure type -- APPLY" procedure))))

;: exercise 4.33
(define (quoted->lazy-list exp env)
    (define (get-lazy-list t)
        (if (null? t)
            '()
            (list 'cons (list 'quote (car t)) (get-lazy-list (cdr t)))))
    (let ((text (text-of-quotation exp)))
        (if (pair? text)
            (eval (get-lazy-list text) env)
            text)))

;: exercise 4.30
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-if exp env)
    (if (true? (actual-value (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (actual-value exp env)
    (display "actual-value: ")
    (print-exp exp)
    (newline)
    (force-it (eval exp env)))

(define (list-of-arg-values exps env)
    (if (no-operands? exps)
        '()
        (cons (actual-value (first-operand exps) env)
              (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
    (if (no-operands? exps)
        '()
        (cons (delay-it (first-operand exps) env)
              (list-of-delayed-args (rest-operands exps) env))))

;: Representing thunks

(define (force-it obj)
    (if (thunk? obj)
        (begin 
            (display "force thunk: ")
            (print-exp (thunk-exp obj))
            (newline)
            (actual-value (thunk-exp obj) (thunk-env obj)))
        obj))

;: Memozied thunk
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
    (cond ((thunk? obj)
            (display "force start => ")
            (print-exp (thunk-exp obj))
            (newline)
            (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
                
                (display "forced thunk: ")
                (print-exp (thunk-exp obj))
                (display " => ")
                (print-exp result)
                (newline)
                
                (set-car! obj 'evaluated-thunk)
                (set-car! (cdr obj) result)
                (set-cdr! (cdr obj) '())
                result))
          ((evaluated-thunk? obj)
            
            (display "forced evaluated-thunk: ")
            (print-exp (thunk-exp obj))
            (display " => ")
            (print-exp (thunk-value obj))
            (newline)

            (thunk-value obj))
          (else obj)))
        

(define (delay-it exp env) 
    (display "delay: ")
    (print-exp exp)
    (newline)
    (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (print-exp exp)
    (if (thunk? exp)
        (begin (display "(thunk ")
               (display (thunk-exp exp))
               (display " <env>)"))
        (user-print exp)))

(define (user-print object)
  (cond ((compound-procedure? object)
            (display (list 'compound-procedure
                           (procedure-parameters object)
                           (procedure-body object)
                           '<procedure-env>)))
        ((thunk? object)
            (print-exp object))
        ((evaluated-thunk? object)
            (print-exp (thunk-value object)))
        (else (display object))))


(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define primitive-procedures
  (list (list 'load load)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'square square)
        ;(list 'map map)
        (list 'newline newline)
        (list 'display display)
        (list 'list list)
;;      more primitives
        ))

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output
                (actual-value input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))


(define the-global-environment (setup-environment))
(actual-value '(define (cons x y) (lambda (m) (m x y))) the-global-environment)
(actual-value '(define (car z) (z (lambda (p q) p))) the-global-environment)
(actual-value '(define (cdr z) (z (lambda (p q) q))) the-global-environment)
(driver-loop)
