;:
;: exercise 4.31
;: Upward-compatible extension for Lazy evalutor
;:
;: how ablut L : (1 2 3 false) ? To ignore it!!
;:

(load "lazy_evaluator.scm")

(define (apply procedure arguments env)  
    (display "-----------")
    (newline)
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
                        (no-flag-proc-parameters (procedure-parameters procedure))
                        (handle-list-of-args (procedure-parameters procedure)
                                             arguments 
                                             env)
                        (procedure-environment procedure))))
          (else (error "Unknown procedure type -- APPLY" procedure))))

(define (delay-it type exp env) 
    (display "delay " )
    (display (list type))
    (display ": ")
    (print-exp exp)
    (newline)
    (list 'thunk type exp env))

(define (thunk-exp thunk) (caddr thunk))
(define (thunk-env thunk) (cadddr thunk))
(define (thunk-value evaluated-thunk) (caddr evaluated-thunk))
(define (thunk-memo? thunk)
    (eq? (cadr thunk) 'lazy-memo))

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
                (if (thunk-memo? obj)
                    (begin
                        (set-car! obj 'evaluated-thunk)
                        (set-car! (cddr obj) result)
                        (set-cdr! (cddr obj) '())
                        result)
                    result)))
          ((evaluated-thunk? obj)
            
            (display "forced evaluated-thunk: ")
            (print-exp (thunk-exp obj))
            (display " => ")
            (print-exp (thunk-value obj))
            (newline)

            (thunk-value obj))
          (else obj)))

(define (last-item L)
    (cond ((not (pair? L)) L)
          ((null? (cdr L)) (car L))
          (else (last-item (cdr L)))))

(define (remove-last-item L)
    (if (null? (cdr L))
        '()
        (cons (car L) (remove-last-item (cdr L)))))

(define (get-flag parameter)
    ;(display "get-flag: ")
    ;(display parameter)
    (let ((item (last-item parameter)))
        ;(display " last-item: ")
        ;(display item)
        ;(newline)
        (cond ((eq? item 'lazy) 'lazy)
              ((eq? item 'lazy-memo) 'lazy-memo)
              (else 'no-lazy))))

(define (no-flag-proc-parameters parameters)
    (display "no-flag-proc-parameters => ")
    (newline)
    (map (lambda (parameter)
            (let ((f (get-flag parameter)))
                (if (eq? f 'no-lazy)
                    parameter
                    (car (remove-last-item parameter))))) 
        parameters))

(define (handle-list-of-args parameters arguments env)
    (newline)
    (display "handle-list-of-args => ")
    
    (if (or (null? parameters) (null? arguments))
        '()
        (let ((para (car parameters))
              (argu (car arguments)))
            (let ((flag (get-flag para)))
        
                (display flag)
                (cond ((eq? flag 'lazy)
                        (cons (delay-it 'lazy argu env)
                              (handle-list-of-args (cdr parameters)
                                                   (cdr arguments)
                                                   env)))
                      ((eq? flag 'lazy-memo)
                        (cons (delay-it 'lazy-memo argu env)
                              (handle-list-of-args (cdr parameters)
                                                   (cdr arguments)
                                                   env)))
                      (else (cons (actual-value argu env)
                                  (handle-list-of-args (cdr parameters)
                                                       (cdr arguments)
                                                       env))))))))

(define input-prompt ";;; Up-L-Eval input:")
(define output-prompt ";;; Up-L-Eval value:")

(define the-global-environment (setup-environment))
(driver-loop)
;: Test
;(define (f a (b lazy) c (d lazy-memo))
;    (+ a b c d))
;(f (+ 1 2) (+ 2 3) (* 1 2) (square 2))
