;:
;:
;:

(load "data_directed_package.scm")
(load "type_index_dispatch.scm")

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly

  (define (make-sparse-poly variable term-list)
    (cons variable 
          ((get 'make 'sparse) term-list)))
  
  (define (make-dense-poly variable term-list)
    (cons variable 
          ((get 'make 'dense) term-list)))
  
  (define (make-poly variable taged-terms)
    (cons variable taged-terms))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? e) (symbol? e))
  (define (same-variable? v1 v2) (and (variable? v1)
                                      (variable? v2)
                                      (eq? v1 v2)))

  ;; representation of terms and term lists
  ;;[procedures adjoin-term ... coeff from text below]

  (define (add-poly p1 p2)

    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  ;: exercise 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-result (div (term-list p1)
                               (term-list p2))))
            (map (lambda (x) (make-poly (variable p1) x)) div-result))
        (error "Polys not in same var -- DIV-PLY" (list p1 p2))))
 
  (define (negtive-poly p)
    (make-poly (variable p)
                (negtive (term-list p))))
  ;: exercise 2.94
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) 
            (gcd-common (term-list p1) (term-list p2)))
        (error "Polys not in same var -- GCD-PLY" (list p1 p2))))
  
  ;: exercise 2.97
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (x) 
                (make-poly (variable p1) x)) 
            (reduce (term-list p1) (term-list p2)))
        (error "Polys not in same var -- reduce-PLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
   
  ;: exercise 2.88
  (put 'negtive '(polynomial) (lambda (p) (tag (negtive-poly p))))
  (put 'sub '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 (negtive-poly p2)))))

  (put 'make-from-sparse 'polynomial
       (lambda (var terms) (tag (make-sparse-poly var terms))))
  (put 'make-from-dense 'polynomial
       (lambda (var terms) (tag (make-dense-poly var terms))))

  ;: exercise 2.87
  (put '=zero? '(polynomial) (lambda (x) (=zero? (term-list x)))) 
  
  (put 'gcd '(polynomial polynomial) 
        (lambda (x y) (tag (gcd-poly x y))))

  (put 'reduce '(polynomial polynomial)
        (lambda (x y) (map tag (reduce-poly x y))))

  (put 'type-index 'polynomial POLYNOMIAL_INDEX)
  (put 'raise '(polynomial) (lambda (x) (tag (make-poly (variable x) (raise (term-list x))))))
  (put 'variable 'polynomial (lambda (x) (variable x)))
  (put 'raise-to-poly 'polynomial (lambda (x y) (tag (make-sparse-poly y (list (list 0 x))))))

  'done)
