;:

(load "../chap3/streams_interfaces.scm")
(load "../chap2/data_directed_package.scm")

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query output:")

(define (query-driver-loop)
    (prompt-for-input input-prompt)
    (let ((q (query-syntax-process (read))))
        (cond ((assertion-to-be-added? q)
                    (add-rule-or-assertion! (add-assertion-body q))
                    (newline)
                    (display "Assertion added to data base.")
                    (query-driver-loop))
              (else
                    (newline)
                    (display output-prompt)
                    (display-stream
                        (stream-map (lambda (frame)
                                        (instantiate q
                                                     frame
                                                     (lambda (v f) (contract-question-mark v))))
                                    (qeval q (singleton-stream '()))))
                    (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
    (define (copy exp)
        (cond ((var? exp)
                    (let ((binding (binding-in-frame exp frame)))
                        (if binding
                            (copy (binding-value binding))
                            (unbound-var-handler exp frame))))
              ((pair? exp)
                    (cons (copy (car exp)) (copy (cdr exp))))
              (else exp)))
    (copy exp))
(define (print-procedure-message msg . objects)
    (display msg)
    (define (print objs)
        (if (null? objs)
           'ok
           (begin
               (display (car objs))
               (display "  ")
               (print (cdr objs)))))
    (print objects)
    (newline)
    )

(define (print-frame frame)
    (display "% Frame => ")
    (display frame)
    (newline)
    (display "---")
    (newline)
    )
(define (print-procedure-message msg . objects)())
(define (print-frame frame) ())

(define (qeval query frame-stream)
    (print-procedure-message "qeval: " query)
    (let ((qproc (get (type query) 'qeval)))
        (if qproc
            (qproc (contents query) frame-stream)
            (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
    (print-procedure-message "simple-query: " query-pattern )
    (print-frame frame-stream)
    (stream-flatmap
        (lambda (frame)
            (stream-append-delayed
                (find-assertions query-pattern frame)
                (delay (apply-rules query-pattern frame))))
        frame-stream))



(define (conjoin conjuncts frame-stream)
    (print-procedure-message "conjoin-query: " conjuncts)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (conjoin (rest-conjuncts conjuncts)
                 (qeval (first-conjunct conjuncts) frame-stream))))

;: exercise 4.76
(define (conjoin conjuncts frame-stream)
    (print-procedure-message "conjoin-query: " conjuncts)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (let ((conjunct1 (first-conjunct conjuncts)))
            (if (empty-conjunction? (rest-conjuncts conjuncts))
                (qeval conjunct1 frame-stream)
                (let ((conjunct2 (first-conjunct (rest-conjuncts conjuncts))))
                    (conjoin (rest-conjuncts (rest-conjuncts conjuncts))
                             (match-conjoin-streams (qeval conjunct1 frame-stream)
                                                    (qeval conjunct2 frame-stream))))))))

(define (match-conjoin-streams frame-stream1 frame-stream2)
    (print-procedure-message "match-conjoin-streams: " frame-stream1 frame-stream2)
    (stream-flatmap
        (lambda (frame1)
            (stream-flatmap (lambda (frame2)
                                (let ((new-frame (match-conjoin-frame frame1 frame2)))
                                    (if (eq? new-frame 'failed)
                                        the-empty-stream
                                        (singleton-stream new-frame))))
                            frame-stream2))
        frame-stream1))

(define (match-conjoin-frame f1 f2)
    ;(newline)
    (print-procedure-message "match-conjoin-frame: " f1 f2)
    (define (iter f new-frame)
        (if (null? f)
            new-frame
            (let ((binding (car f)))
                (let ((f2-binding (binding-in-frame (binding-variable binding) f2)))
                    (if f2-binding
                        (if (equal? (binding-value f2-binding) (binding-value binding))
                            (iter (cdr f) new-frame) ; matched
                            'failed) ; match failed
                        (iter (cdr f) (extend (binding-variable binding)
                                              (binding-value binding)
                                              new-frame)))))))
    (iter f1 f2))

(define (disjoin disjuncts frame-stream)
    (print-procedure-message "disjoin-query: " disjuncts)
    (if (empty-disjunction? disjuncts)
        frame-stream
        (interleave-delayed
            (qeval (first-disjunct disjuncts) frame-stream)
            (delay (disjoin (rest-conjuncts disjuncts) frame-stream)))))

(define (negate operands frame-stream)
    (print-procedure-message "negate-query: " operands)
    (stream-flatmap
        (lambda (frame)
            (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
                (singleton-stream frame)
                the-empty-stream))
        frame-stream))

(define (lisp-value call frame-stream)
    (print-procedure-message "lisp-value-query: " call)
    (stream-flatmap
        (lambda (frame)
            (if (execute (instantiate call
                                      frame
                                      (lambda (v f)
                                            (error "Unknown pat var -- LISP-VALUE" v))))
                (singleton-stream frame)
                the-empty-stream))
        frame-stream))

(define (execute exp)
    (print-procedure-message "execute: " exp)
    (apply (eval (predicate exp) user-initial-environment)
           (args exp)))

(define (always-true ignore frame-stream) frame-stream)

;: exercise 4.75
(define (uniquely-asserted operands frame-stream)
    (print-procedure-message "uniquely-asserted: " operands)
    (stream-flatmap
        (lambda (frame)
            (let ((eval_result (qeval (car operands) (singleton-stream frame))))
                (print-procedure-message "=> Qeval unique contents finished: " eval_result)
                ;(display (stream-cdr eval_result))
                ;(newline)
                (if (stream-null? eval_result)
                    (singleton-stream frame)
                    (if (stream-null? (stream-cdr eval_result))
                        (singleton-stream (stream-car eval_result))
                        the-empty-stream))))
        frame-stream))

(define (find-assertions pattern frame)
    (print-procedure-message "find-assertions: " pattern)
    (stream-flatmap
        (lambda (datum)
            ;(newline)
            (print-procedure-message "Start check-an-assertion => " datum)
            (check-an-assertion datum pattern frame))
        (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
    (let ((match-result (pattern-match query-pat assertion query-frame)))
        (print-procedure-message "=> Check-an-assertion finished: " assertion match-result)
        (if (eq? match-result 'failed)
            the-empty-stream
            (singleton-stream match-result))))

(define (pattern-match pat dat frame)
    (print-procedure-message "pattern-match: " pat dat)
    ;(print-frame frame)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? pat dat) frame)
          ((var? pat) (extend-if-consistent pat dat frame))
          ((and (pair? pat) (pair? dat))
                (pattern-match (cdr pat)
                               (cdr dat)
                               (pattern-match (car pat)
                                              (car dat)
                                              frame)))
          (else 'failed)))

(define (extend-if-consistent var dat frame)
    (print-procedure-message "extend-if-consistent: " var dat)
    (let ((binding (binding-in-frame var frame)))
        (if binding
            (pattern-match (binding-value binding) dat frame)
            (extend var dat frame))))

(define (apply-rules pattern frame)
    ;(newline)
    (print-procedure-message "apply-rules: " pattern)
    (stream-flatmap
        (lambda (rule)
            (apply-a-rule rule pattern frame))
        (fetch-rules pattern frame)))
(define (apply-a-rule rule query-pattern query-frame)
    (print-procedure-message "apply-a-rule: " rule query-pattern)
    (let ((clean-rule (rename-variables-in rule)))
        (let ((unify-result (unify-match query-pattern
                                         (conclusion clean-rule)
                                         query-frame)))
            (if (eq? unify-result 'failed)
                the-empty-stream
                (qeval (rule-body clean-rule)
                       (singleton-stream unify-result))))))

(define (rename-variables-in rule)
    (let ((rule-application-id (new-rule-application-id)))
        (define (tree-walk exp)
            (cond ((var? exp) (make-new-variable exp rule-application-id))
                  ((pair? exp)
                        (cons (tree-walk (car exp))
                              (tree-walk (cdr exp))))
                  (else exp)))
        (tree-walk rule)))

(define (unify-match p1 p2 frame)
    (print-procedure-message "unify-match: " p1 p2)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame))
          ((and (pair? p1) (pair? p2))
                (unify-match (cdr p1)
                             (cdr p2)
                             (unify-match (car p1)
                                          (car p2)
                                          frame)))
          (else 'failed)))

(define (extend-if-possible var val frame)
    (print-procedure-message "extend-if-possible: ")
    (let ((binding (binding-in-frame var frame)))
        (cond (binding (unify-match (binding-value binding) val frame))
              ((var? val)
                (let ((binding (binding-in-frame val frame)))
                    (if binding
                        (unify-match var (binding-value binding) frame)
                        (extend var val frame))))
              ((depends-on? val var frame)
                'failed)
              (else (extend var val frame)))))

(define (depends-on? exp var frame)
    (define (tree-walk e)
        (cond ((var? e)
                    (if (equal? e var)
                        true
                        (let ((b (binding-in-frame e frame)))
                            (if b
                                (tree-walk (binding-value b))
                                false))))
              ((pair? e)
                (or (tree-walk (car e))
                    (tree-walk (cdr e))))
              (else false)))
    (tree-walk exp))

;: assertions
(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
    (if (use-index? pattern)
        (get-indexed-assertions pattern)
        (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
    (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
    (let ((s (get key1 key2)))
        (if s s the-empty-stream)))

;; rules
(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
    (if (use-index? pattern)
        (get-indexed-rules pattern)
        (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
    (stream-append (get-stream (index-key-of pattern) 'rule-stream)
                   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
    (if (rule? assertion)
        (add-rule! assertion)
        (add-assertion! assertion)))

(define (add-assertion! assertion)
    (store-assertion-in-index assertion)
    (let ((old-assertions THE-ASSERTIONS))
        (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
        'ok))

(define (add-rule! rule)
    (store-rule-in-index rule)
    (let ((old-rules THE-RULES))
        (set! THE-RULES (cons-stream rule old-rules))
        'ok))

(define (store-assertion-in-index assertion)
    (print-procedure-message "store-assertion-in-index: " assertion)
    (if (indexable? assertion)
        (let ((key (index-key-of assertion)))
            (let ((current-assertion-stream (get-stream key 'assertion-stream)))
                (put key 'assertion-stream (cons-stream assertion current-assertion-stream))))))

(define (store-rule-in-index rule)
    (print-procedure-message "store-rule-in-index: " rule)
    (let ((pattern (conclusion rule)))
        (if (indexable? pattern)
            (let ((key (index-key-of pattern)))
                (let ((current-rule-stream (get-stream key 'rule-stream)))
                    (put key 'rule-stream (cons-stream rule current-rule-stream)))))))

(define (indexable? pat)
    (or (constant-symbol? (car pat))
        (var? (car pat))))
(define (index-key-of pat)
    (let ((key (car pat)))
        (if (var? key) '? key)))
(define (use-index? pat)
    (constant-symbol? (car pat)))

;: streams' operations

(define (stream-append-delayed s1 delayed-s2)
    (if (stream-null? s1)
        (force delayed-s2)
        (cons-stream (stream-car s1) (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
    (if (stream-null? s1)
        (force delayed-s2)
        (cons-stream (stream-car s1)
                     (interleave-delayed (force delayed-s2)
                                         (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
    (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
    (if (stream-null? stream)
        the-empty-stream
        (interleave-delayed (stream-car stream)
                            (delay (flatten-stream (stream-cdr stream))))))
(define (singleton-stream x)
    (cons-stream x the-empty-stream))

;: syntax procedures
(define (type exp)
    (if (pair? exp)
        (car exp)
        (error "Unknown expression TYPE" exp)))
(define (contents exp)
    (if (pair? exp)
        (cdr exp)
        (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
    (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
    (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
    (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
    (if (null? (cddr rule))
        '(always-true)
        (caddr rule)))

(define (query-syntax-process exp)
    (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
    (cond ((pair? exp)
                (cons (map-over-symbols proc (car exp))
                      (map-over-symbols proc (cdr exp))))
          ((symbol? exp) (proc exp))
          (else exp)))

(define (expand-question-mark symbol)
    (let ((chars (symbol->string symbol)))
        (if (string=? (substring chars 0 1) "?")
            (list '? (string->symbol (substring chars 1 (string-length chars))))
            symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id)
    (set! rule-counter (+ 1 rule-counter))
    rule-counter)

(define (make-new-variable var rule-application-id)
    (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
    (string->symbol (string-append "?"
                        (if (number? (cadr variable))
                            (string-append (symbol->string (caddr variable))
                                           "-"
                                           (number->string (cadr variable)))
                            (symbol->string (cadr variable))))))

;; frame and bindings

(define (make-binding variable value)
    (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
    (assoc variable frame))
(define (extend variable value frame)
    (cons (make-binding variable value) frame))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define get '())

(define put '())

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)

  ;: exercise 4.75
  (put 'unique 'qeval uniquely-asserted)

  (deal-out rules-and-assertions '() '()))

(define microshaft-data-base
  '(
;; from section 4.4.1
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(can-do-job (administration secretary)
            (administration big wheel))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;: exercise 4.58
(rule (big-shot ?x)
    (and (job ?x (?p . ?t))
         (supervisor ?x ?y)
         (not (job ?y (?p . ?type)))))
))
(initialize-data-base microshaft-data-base)
(query-driver-loop)
