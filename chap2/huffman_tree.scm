;:
;:

;: leaf's constructor and selector
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? object)
    (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;: code tree's constructor and selector
(define (make-code-tree left right)
    (list left
          right
          (append (symbol left) (symbol right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbol tree) (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree) (if (leaf? tree) (weight-leaf tree) (cadddr tree)))


(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

;: ((A 4) (B 2) (C 1) (D 1))
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs))))))

;: exercise 2.69
(define (generate-huffman-tree pairs)
    (define (successive-merge p)
        (if (null? (cdr p)) 
            (car p)
            (let ((new-tree (make-code-tree (car p) (cadr p))))
                (successive-merge (adjoin-set new-tree (cddr p))))))
    (successive-merge (make-leaf-set pairs)))

;: Test
(define x1 (list (list 'C 1) (list 'B 2) (list 'A 4) (list 'D 1)))
(define t1 (make-leaf-set x1))
(define t2 (generate-huffman-tree x1))
