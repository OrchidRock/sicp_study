
;: exercise 4.62
(assert! (rule (last-pair (?u ?v) (?v))))
(assert! (rule (last-pair (?u) (?u))))
(assert! (rule (last-pair (?u . ?v) (?x))
               (last-pair ?v (?x))))
