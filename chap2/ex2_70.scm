;:
;: exercise 2.70

(load "huffman_tree.scm")
(load "huffman_coder.scm")

(define rock-songs (list (list 'A 2) (list 'BOOM 1)
                         (list 'NA 16) (list 'SHA 3)
                         (list 'GET 2) (list 'JOB 2)
                         (list 'YIP 9) (list 'WAH 1)))

(define ht (generate-huffman-tree rock-songs))
(define m1 '(Get a job
             Sha na na na na na na na na
             Get a job
             Sha na na na na na na na na
             Wah yip yip yip yip yip yip yip yip
             Sha boom))
(define t1 (encode m1 ht))
