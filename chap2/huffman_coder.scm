;
;:
(load "huffman_tree.scm")

(define (decode bits tree)
    (define (chooes-branch bit branch)
        (cond ((= bit 1) (right-branch branch))
              ((= bit 0) (left-branch branch))
              (else (error "bad bit -- CHOOSE-BRANCH" bit))))
    (define (decode-1 bits current-branch)
        (if (null? bits) 
            '()
            (let ((next-branch (chooes-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

;: exercise 2.68
(define (encode-symbol x tree)
    (cond ((leaf? tree) '())
          ((memq x (symbol (left-branch tree)))
            (cons 0 (encode-symbol x (left-branch tree))))
          ((memq x (symbol (right-branch tree)))
            (cons 1 (encode-symbol x (right-branch tree))))
          (else (error "bad symbol -- ENCODE-SYMBOL" x))))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

;: Test
;: exercise 2.67
(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define m1 '(d a c b d b c a))
(define t1 (encode m1 sample-tree))
