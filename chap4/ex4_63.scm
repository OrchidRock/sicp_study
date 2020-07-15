;:
;: exercise 4.63
;:

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

(assert! (rule (grand-son ?x ?y)
               (or (and (son ?x ?a)
                        (son ?a ?y))
                   (and (son ?x ?a)
                        (law-son ?a ?y))
                   (and (law-son ?x ?a)
                        (son ?a ?y))
                   (and (law-son ?x ?a)
                        (law-son ?a ?y)))))
