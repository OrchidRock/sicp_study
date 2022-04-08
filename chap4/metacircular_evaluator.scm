

;: exercise 4.3
;: Rewrite eval so that the dispatch is done in data-directed style.
;(load "../chap2/ata_directed_package.scm")

(define apply-in-underlying-scheme apply)


(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          (else ((get 'eval (car exp)) exp env))))

(define (eval exp env)
  (display "eval: ")
  (display exp)
  ;(display "pair?: " (application? exp))
  (newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
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
        ((or? exp) (eval (or->if exp) env))

        ;: exercise 4.6
        ((let? exp) (eval (let->combination exp) env))

        ;; exercise 4.7
        ((let*? exp) (eval (let*->nested-lets exp) env))

        ;: exercise 4.9
        ((while? exp) (eval (while->combination exp) env))

        ;: exercise 4.25
        ((unless? exp) (eval (unless->if exp) env))

        ((application? exp)
         ;(display "lv: " (list-of-values (operands exp) env))
         ;(newline)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
                (apply-primitive-procedure procedure arguments))
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
                        arguments
                        (procedure-environment procedure))))
          (else (error "Unknown procedure type -- APPLY" procedure))))

;:
;: we can rewrite list-of-values to implement the "simultaneous" scope rule.
;: But we have to consider the procedure, which includes mutually recursive internal
;: definitions.
(define (error-no-exception msg . others)
    (display msg)
    (display " ")
    (display others)
    (newline)
    -1)
(define (list-of-values-new exps env) ())

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;: exercise 4.1
;: a version of list-of-values that evaluates operands from left to right regardless of the
;: order of evalution in the underlying Lisp.
(define (list-of-values-left-to-right exps env)
    (define (iter e results)
        (if (no-operands? e)
            results
            (let ((left-value (eval (first-operand e) env)))
                (iter (rest-operands e) (append results (list left-value))))))
    (iter exps '()))
(define (list-of-values-left-to-right2 exps env)
    (if (no-operands? exps)
        '()
        (let ((left-value (eval (first-operand exps) env)))
            (cons left-value (list-of-values-left-to-right2
                                    (rest-operands exps)
                                    env)))))

;: a version of list-of-values that evaluates operands from right to left regardless of the
;: order of evalution in the underlying Lisp.
(define (list-of-values-right-to-left exps env)
    (if (no-operands? exps)
        '()
        (let ((right-value (list-of-values-right-to-left (rest-operands exps) env)))
            (append right-value (list (eval (first-operand exps) env))))))


(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((eq? exp '*unassigned*) '*unassigned*)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (install-quoted-package)
    (define (text-of-quotation-env exp env)
        (text-of-quotation exp))
    (put 'eval 'quote text-of-quotation-env))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (make-assignment var val)
    (cons 'set! (list var val)))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (install-assignment-package)
    (put 'eval 'set! eval-assignment))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
;: exercise 4.8
(define (make-definition variable parameters body)
    (cons 'define (cons (cons variable parameters)
                        body)))

(define (install-definition-package)
    (put 'eval 'define eval-definition))
(define (install-lambda-package)
    (define (make-procedure-from-lambda exp env)
        (make-procedure (lambda-parameters exp) (lambda-body exp) env))
    (put 'eval 'lambda make-procedure-from-lambda))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (install-if-package)
    (put 'eval 'if eval-if))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (install-begin-package)
    (define (my-begin exp env)
        (eval-sequence (begin-actions exp) env))
    (put 'eval 'begin my-begin))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;: exercise 4.2
;: (call <operator> <operands>)
;(define (application? exp) (tagged-list? exp 'call))
;(define (operator exp) (cadr exp))
;(define (operands exp) (cddr exp))

(define (install-application-package)
    (define (my-application exp env)
        (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
    (put 'eval 'call my-application))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (install-cond-package)
    (define (my-cond exp env)
        (eval (cond->if exp) env))
    (put 'eval 'cond my-cond))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

;: exercise 4.5
(define (cond-form2-clause? clause)
    (eq? (car (cond-actions clause)) '=>))

;; only one action which has only one parameter
(define (cond-form2-action clause)
    (cadr (cond-actions clause)))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))

            ;: exercise 4.5
            (if (and (cond-form2-clause? first)
                     (cond-predicate first))
                (list (cond-form2-action first) (cond-predicate first));
                ;(eval (cond-form2-action first) (cond-predicate first))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

;; exercise 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (eval-and exp env)
    (define (eval-and-caluses caluses)
        (if (null? caluses)
            'true
            (if (eval (car caluses) env)
                (eval-and-caluses (cdr caluses))
                'false)))
    (eval-and-caluses (cdr exp)))

(define (eval-or exp env)
    (define (eval-or-caluses caluses)
        (if (null? caluses)
            'false
            (if (eval (car caluses) env)
                'true
                (eval-or-caluses (cdr caluses)))))
    (eval-or-caluses (cdr exp)))

(define (and->if exp)
    (define (expand-and-caluses caluses)
        (if (null? caluses)
            'true
            (make-if (car caluses)
                     (expand-and-caluses (cdr caluses))
                     'false)))
    (expand-and-caluses (cdr exp)))

(define (or->if exp)
    ;(display "or->if: ")
    ;(display exp)
    ;(newline)
    (define (expand-or-caluses caluses)
        ;(display "caluses: ")
        ;(display caluses)
        ;(newline)
        (if (null? caluses)
            'false
            (make-if (car caluses)
                     'true
                     (expand-or-caluses (cdr caluses)))))
    (expand-or-caluses (cdr exp)))

(define (install-or-package)
    (put 'eval 'or eval-or))
(define (install-and-package)
    (put 'eval 'and eval-and))

;; exercise 4.7
(define (let*? exp) (tagged-list? exp 'let*))
(define (make-let parameters-and-exps body)
    (cons 'let (cons parameters-and-exps body)))

(define (let*->nested-lets exp)
    (define (translate caluses)
        (if (null? caluses)
            (let-body exp)
            (make-let (list (car caluses))
                      (translate (cdr caluses)))))
    (translate (let-vars-and-exps exp)))

;: exercise 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-vars-and-exps exp) (cadr exp))
(define (let-vars caluses)
    (if (null? caluses)
        '()
        (cons (car (car caluses))
              (let-vars (cdr caluses)))))
(define (let-exps caluses)
    (if (null? caluses)
        '()
        (cons (cadr (car caluses))
              (let-exps (cdr caluses)))))

;; exercise 4.8
(define (named-let? exp) (variable? (cadr exp)))

(define (let->combination exp)
    (if (named-let? exp)
        (make-begin (list (make-definition (cadr exp)
                               (let-vars (caddr exp))
                               (cdddr exp)))
                          (cons (cadr exp) (let-exps (caddr exp)))))
        (cons (make-lambda (let-vars (let-vars-and-exps  exp))
                           (let-body exp))
              (let-exps (let-vars-and-exps exp))))


(define (install-let-package)
    (define (my-let exp env)
        (eval (let->combination exp) env))
    (put 'eval 'let my-let))

;: exercise 4.9
(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
;(define (make-while predicate body)
;    (cons 'while (cons predicate body)))

(define (while->combination exp)
    (make-if (while-predicate exp)
             (make-begin (append (while-body exp) (list exp)))
             'true))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))
(define (unless->if exp)
    (make-if (unless-condition exp)
             (unless-exceptional-value exp)
             (unless-usual-value exp)))

;: evaluator data structures

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


;: exercise 4.17
;: scan-out-defines constructes a extra frame.
;: Design a way without constructing it:
;: To translate all internal definitations to lambda expressions will create new problem
;: that which breaks the order of set!.

(define (scan-out-defines-optim proc-body)
    (define (iter body define-caluses set-caluses exp-calues)
        (if (null? body)
            (cons define-caluses (append set-caluses exp-calues))
            (let ((first-exp (car body)))
                (if (definition? first-exp)
                    (iter (cdr body)
                          (append define-caluses (list (list 'define
                                                             (definition-variable first-exp)
                                                             '*unassigned*)))
                          (append set-caluses (list (make-assignment (definition-variable first-exp)
                                                 (definition-value first-exp))))
                          exp-calues)
                    (iter (cdr body)
                          define-caluses
                          set-caluses
                          (append exp-calues (list first-exp)))))))
    (let ((items (iter proc-body '() '() '())))

        (if (null? (car items))
            proc-body
            (append (car items) (cdr items)))))

;: exercise 4.16 b)
(define (scan-out-defines proc-body)
    (define (iter body let-caluses set-caluses exp-calues)
        (if (null? body)
            (cons let-caluses (append set-caluses exp-calues))
            (let ((first-exp (car body)))
                (if (definition? first-exp)
                    (iter (cdr body)
                          (append let-caluses (list (list (definition-variable first-exp)
                                                          '*unassigned*)))
                          (append set-caluses (list (make-assignment (definition-variable first-exp)
                                                 (definition-value first-exp))))
                          exp-calues)
                    (iter (cdr body)
                          let-caluses
                          set-caluses
                          (append exp-calues (list first-exp)))))))
    (let ((items (iter proc-body '() '() '())))

        (if (null? (car items))
            proc-body
            (list (make-let (car items) (cdr items))))))

;:
;: scan-out-defines can't match the analyzing_mceval.scm
;:
(define (make-procedure parameters body env)
    (display "make-procedure scan-out-defines: ")
    (display body)
    (newline)
    (display "           after: ")
    ;(display (scan-out-defines-optim body))
    (newline)
    ; exercise 4.16 c)
    ;(list 'procedure parameters (scan-out-defines-optim body) env))
    ;(list 'procedure parameters (scan-out-defines body) env))
    (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;: environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (display "extend-environment: ")
  (display vars)
  (display "  ")
  (user-print-objects vals)
  (newline)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (display "lookup-variable-value: ")
  (display var)
  (display " => ")
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*) ;: exercise 4.16 a)
                (error "LOOKUP-VARIABLE-VALUE have found unassigned variable" var)
                (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (let ((return-val (env-loop env)))
    (user-print return-val)
    (newline)
    return-val))

(define (set-variable-value! var val env)
  (display "set-variable-value!: ")
  (display var)
  (display " ")
  (user-print val)
  (newline)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (display "define-variable!: ")
  (display var)
  (display " ")
  (user-print val)
  (newline)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;: run
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list 'square square)
        (list 'quit quit)
        (list 'list list)
;;      more primitives
        ))

;: exercise 4.14
;: if we input: (map car (list (list 1 2) (list 2 3)))
;: the map object of system: #[compiled-procedure 14 (list #x6f)]
;: the car object of system: #[compiled-procedure 15 (list #x1)]
;: then we apply: (apply-in-underlying-scheme #[compiled-procedure 14 (list #6xf)]
;:                                            ('primitive #[compiled-procedure 15 (list #x1)])
;:                                            ((1 2) (3 4)))
;: the system's map can't identify the ('primitive #[compiled-procedure 15 (list #x1)])
;: because it's the defination of ourself.
;: so we have to define our own map procedure.

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map (lambda (proc) (list 'primitive (cadr proc)))
                                           primitive-procedures))

(define (apply-primitive-procedure proc args)
    (display "apply-primitive-procedure: ")
    (display proc)
    (display "  ")
    (user-print-objects args)
    (newline)
    (apply-in-underlying-scheme (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print-objects objects)
    (cond ((pair? objects)
               (user-print (car objects))
               (display " ")
               (user-print-objects (cdr objects)))
          ((null? objects) 'ok)
          (else (user-print objects))))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;(define the-global-environment (setup-environment))
;(driver-loop)

; (install-quoted-package)
; (install-assignment-package
; (install-definition-package)
; (install-lambda-package)
; (install-if-package)
; (install-begin-package)
; (install-application-package)
; (install-cond-package)
; (install-or-package)
; (install-and-package)
