
;: exercise 4.68


(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x) (?x))))
(assert! (rule (reverse (?a ?b) (?b ?a))))
(assert! (rule (reverse (?x . ?y) ?z) (and (append-to-form ?c (?x) ?z) (reverse ?y ?c))))
