
;:
;: Amb Evaluator

(load "metacircular_evaluator.scm")
(cd "../chap1")
(load "prime.scm")
(cd "../chap4")

(define (ambeval exp env succeed fail)
    (display "ambeval: ")
    (display exp)
    (display " <env> ")
    (display succeed)
    (display " ")
    (display fail)
    (newline)
    ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-perm-assignment exp)) ;; exercise 4.51
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))

        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))

        ((let? exp) (analyze-let exp))                      ;: exercise 4.22
        ((unless? exp) (analyze (unless->if exp)))          ;: exercise 4.26

        ((amb? exp) (analyze-amb exp))

        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (print-analyze msg . objects)
    (display msg)
    (if (null? objects)
        'ok
        (display objects))
    (newline))


(define (analyze-self-evaluating exp)
    (print-analyze "Analyze-self-evaluating <=> " exp)
    (lambda (env succeed fail)
        (succeed exp fail)))
(define (analyze-quoted exp)
    (print-analyze "Before analyze-quoted => " exp)
    (let ((qval (text-of-quotation exp)))
        (print-analyze "=> After analyze-quoted: lambda (env succeed fail)" qval)
        (lambda (env succeed fail)
            (succeed qval fail))))

(define (analyze-variable exp)
    (print-analyze "Analyze-variable <=> " exp)
    (lambda (env succeed fail)
        (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
    (print-analyze "Before analyze-lambda => " exp)
    (let ((vars (lambda-parameters exp))
          (bproc (analyze-sequence (lambda-body exp))))
        (print-analyze "=> After analyze-lambda: lambda (env succeed fail) " vars bproc)
        (lambda (env succeed fail)
            (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
    (print-analyze "Before analyze-if => " exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
        (print-analyze "=> After analyze-if: lambda (env succeed fail) " pproc cproc aproc)
        (lambda (env succeed fail)
            (pproc env
                   (lambda (pred-value fail2)
                        (if (true? pred-value)
                            (cproc env succeed fail2)
                            (aproc env succeed fail2)))
                    fail))))

(define (analyze-sequence exps)
    (print-analyze "Before analyze-sequence => " exps)
    (define (sequentially a b)
        (lambda (env succeed fail)
            (a env
                (lambda (a-value fail2)
                    (b env succeed fail2))
                fail)))
    (define (loop first-proc rest-procs)
        (print-analyze "  analyze-sequence loop => first-proc: " first-proc)
        (if (null? rest-procs)
            (begin (print-analyze "=> After analyze-sequence: " first-proc)
                first-proc)
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (print-analyze  "  Before analyze-sequence loop => " procs)
        (if (null? procs)
            (error "Empty sequence -- ANALYZE"))
        (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
    (print-analyze "Before analyze-definition => " exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
        (print-analyze "=> After analyze-definition: lambda (env succeed fail) " var vproc)
        (lambda (env succeed fail)
            (vproc env
                    (lambda (val fail2)
                        (define-variable! var val env)
                        (succeed 'ok fail2))
                   fail))))
(define (analyze-assignment exp)
    (print-analyze "Before analyze-assignment => " exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
        (print-analyze "=> After analyze-assignment: lambda (env) " var vproc)
        (lambda (env succeed fail)
            (vproc env
                    (lambda (val fail2)
                        (let ((old-vlue (lookup-variable-value var env)))
                            (set-variable-value! var val env)
                            (succeed 'ok
                                     (lambda ()
                                        (set-variable-value! var old-vlue env)
                                        (fail2)))))
                    fail))))

;: exercise 4.51
(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))
(define (analyze-perm-assignment exp)
    (print-analyze "Before analyze-perm-assignment => " exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
        (print-analyze "=> After analyze-perm-assignment: lambda (env) " var vproc)
        (lambda (env succeed fail)
            (vproc env
                    (lambda (val fail2)
                        (set-variable-value! var val env)
                        (succeed 'ok fail2))
                    fail))))


(define (analyze-let exp)
    (print-analyze "Before analyze-let => " exp)
    (let ((proc (analyze (let->combination exp))))
        (print-analyze "=> After analyze-let: lambda (env succeed fail) " proc)
        proc))
        ;(lambda (env succeed fail)
        ;    (proc env succeed fail))))

(define (analyze-application exp)
    (print-analyze "Before analyze-application => " exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
        (print-analyze "=> After analyze-application: lambda (env succeed fail) " fproc aprocs)
        (lambda (env succeed fail)
            (fproc env
                    (lambda (proc fail2)
                        (get-args aprocs env (lambda (args fail3)
                                                (execute-application proc args succeed fail3))
                                             fail2))
                    fail))))
(define (get-args aprocs env succeed fail)
    (print-analyze "Before get-args => " aprocs)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                      (lambda (arg fail2)
                            (get-args (cdr aprocs)
                                      env
                                      (lambda (args fail3)
                                        (print-analyze "=> After get-args: " args)

                                        (succeed (cons arg args) fail3))
                                       fail2))
                       fail)))

(define (execute-application proc args succeed fail)
    (cond ((primitive-procedure? proc)
            (succeed (apply-primitive-procedure proc args) fail))
          ((compound-procedure? proc)
            ((procedure-body proc)
                (extend-environment (procedure-parameters proc)
                                    args
                                    (procedure-environment proc))
                 succeed
                 fail))
          (else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
(define (analyze-amb exp)
    (print-analyze "Before analyze amb => ")
    (let ((cprocs (map analyze (amb-choices exp))))
        (print-analyze "=> After analyze amb:  " cprocs)
        (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    ((car choices) env
                                   succeed
                                   (lambda () (try-next (cdr choices))))))
            (try-next cprocs))))


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (get-input)
    (if (null? pre-eval-procedures)
        (read)
        (let ((first-proc (car pre-eval-procedures)))
            (set! pre-eval-procedures (cdr pre-eval-procedures))
            first-proc)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (get-input)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

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
        (list '<= <=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
        (list 'prime? prime?) ;: load prime.scm
        (list 'display display)
        (list 'newline newline)
        (list 'load load)
        ;(list 'and and)
        ;(list 'or or)
        ;(list 'read-string read-string)
        ;(list 'open-i/o-file open-i/o-file)
;;      more primitives
        ))

(define pre-eval-procedures
    (list '(define (require p) (if (not p) (amb)))))

(define the-global-environment (setup-environment))
(driver-loop)
