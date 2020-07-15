;: exercise 4.69

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (son ?x ?y)
               (and (wife ?x ?z)
                    (son ?z ?y))))

(assert! (rule (grandson ?x ?y)
               (and (son ?x ?a)
                    (son ?a ?y))))

(assert! (rule ((great grandson) ?x ?y)
               (and (son ?x ?z)
                    (grandson ?z ?y))))

(assert! (rule (last-grandson (grandson))))
(assert! (rule (last-grandson (great . ?y)) (last-grandson ?y)))

;(assert! (rule (gr)))


(assert! (rule ((great . ?rel) ?x ?y)
               (and (son ?x ?z)
                    (?rel ?z ?y)
                    (last-grandson ?rel))))
