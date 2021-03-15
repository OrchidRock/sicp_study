
(declare (usual-integrations))

(define (pascal_triangle x y)
    (cond ((< x y) 0)
          ((<= y 0) 0)
          ((<= x 0) 0)
          ((and (= x 1) (= y 1)) 1)
          (else (+ (pascal_triangle (- x 1)
                                    (- y 1))
                   (pascal_triangle (- x 1)
                                    y)))))


