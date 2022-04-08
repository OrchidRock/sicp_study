(load "reg_machine_simulator.scm")
(define fact-machine
    (make-machine
        '(n val continue)
        (list (list '* *) (list '= =) (list '- -))
        '(controller
            (assign continue (label fact-done))
          fact-loop
            (test (op =) (reg n) (const 1))
            (branch (label base-case))
            (save continue)
            (save n)
            (assign n (op -) (reg n) (const 1))
            (assign continue (label after-fact))
            (goto (label fact-loop))
          after-fact
            (restore n)
            (restore continue)
            (assign val (op *) (reg n) (reg val))
            (goto (reg continue))
          base-case
            (assign val (const 1))
            (goto (reg continue))
          fact-done)
     ))

(set-register-contents! fact-machine 'n 5)
(fact-machine 'trace-on)
(trace-register-on fact-machine 'val)
(trace-register-on fact-machine 'n)
(set-breakpoint fact-machine 'fact-loop 0)
(start fact-machine)
;(display (get-register-contents fact-machine 'val))
;(newline)
;(display (fact-machine 'print-pc-count))
