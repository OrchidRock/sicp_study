
;: exercise 4.61
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
               (?x next-to ?y in ?z)))

