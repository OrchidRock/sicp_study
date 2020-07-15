;:
;: exercise 4.58
;:

(rule (big-shot ?x)
    (and (job ?x (?p . ?t))
         (supervisor ?x ?y)
         (not (job ?y (?p . ?type)))))
