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

(assert! (rule (law-son ?x ?y)
               (and (wife ?x ?z)
                    (son ?z ?y))))

(assert! (rule (grandson ?x ?y)
               (or (and (son ?x ?a)
                        (son ?a ?y))
                   (and (son ?x ?a)
                        (law-son ?a ?y))
                   (and (law-son ?x ?a)
                        (son ?a ?y))
                   (and (law-son ?x ?a)
                        (law-son ?a ?y)))))

(assert! (rule ((great grandson) ?x ?y)
               (and (or (son ?x ?z)
                        (law-son ?x ?z))
                    (grandson ?z ?y))))

(assert! (rule (last-grandson (grandson))))
;(assert! (rule (last-grandson (great grandson))))
(assert! (rule (last-grandson (great . ?y)) (last-grandson ?y)))


(assert! (rule ((great . ?rel) ?x ?y)
               (and (or (son ?x ?z)
                        (law-son ?x ?z))
                    (?rel ?z ?y)
                    (last-grandson ?rel))))
