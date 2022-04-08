;:
;: Separating syntactic analysis from execution
;:

(define apply-in-underlying-scheme apply)
(load "metacircular_evaluator.scm")

(define (eval exp env)
    ;(let ((analyzed-proc (analyze exp)))
        ;(print-analyze "analyzed: " analyzed-proc)
        ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-let exp))                      ;: exercise 4.22
        ((unless? exp) (analyze (unless->if exp)))          ;: exercise 4.26
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
    (lambda (env) exp))

(define (analyze-quoted exp)
    (print-analyze "Before analyze-quoted => " exp)
    (let ((qval (text-of-quotation exp)))
        (print-analyze "=> After analyze-quoted: lambda (env)" qval)
        (lambda (env) qval)))

(define (analyze-variable exp)
    (print-analyze "Analyze-variable <=> " exp)
    ;(let ((val (lookup-variable-value exp env)))
        ;(print-analyze "analyze-variable: lambda (env)" val)
    (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
    (print-analyze "Before analyze-assignment => " exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
        (print-analyze "=> After analyze-assignment: lambda (env) " var vproc)
        (lambda (env)
            (set-variable-value! var (vproc env) env)
            'ok)))

(define (analyze-definition exp)
    (print-analyze "Before analyze-definition => " exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
        (print-analyze "=> After analyze-definition: lambda (env) " var vproc)
        (lambda (env)
            (define-variable! var (vproc env) env))))

(define (analyze-if exp)
    (print-analyze "Before analyze-if => " exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
        (print-analyze "=> After analyze-if: lambda (env) " pproc cproc aproc)
        (lambda (env)
            (if (true? (pproc env))
                (cproc env)
                (aproc env)))))

(define (analyze-lambda exp)
    (print-analyze "Before analyze-lambda => " exp)
    (let ((vars (lambda-parameters exp))
          (bproc (analyze-sequence (lambda-body exp))))
        (print-analyze "=> After analyze-lambda: lambda (env) " vars bproc)
        (lambda (env) (make-procedure vars bproc env))))


(define (analyze-sequence exps)
    (print-analyze "Before analyze-sequence => " exps)
    (define (sequentially proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env)))
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

;: exercise 4.22
(define (analyze-let exp)
    (print-analyze "Before analyze-let => " exp)
    (let ((proc (analyze (let->combination exp))))
        (print-analyze "=> After analyze-let: lambda (env) " proc)
        (lambda (env) (proc env))))

(define (analyze-application exp)
    (print-analyze "Before analyze-application => " exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
        (print-analyze "=> After analyze-application: lambda (env) " fproc aprocs)
        (lambda (env)
            (execute-application (fproc env)
                                 (map (lambda (aproc) (aproc env))
                                    aprocs)))))

(define (execute-application proc args)
    (print-analyze "Execute-application => ... ")
    (cond ((primitive-procedure? proc)
            (apply-primitive-procedure proc args))
          ((compound-procedure? proc)
            ((procedure-body proc)
             (extend-environment (procedure-parameters proc)
                                 args
                                 (procedure-environment proc))))
         (else
            (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define the-global-environment (setup-environment))
(driver-loop)
