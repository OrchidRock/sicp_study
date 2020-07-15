;
;: exercise 3.56
;:

(load "streams.scm")


(define S (cons-stream 1 (merge-stream (scale-stream S 2) 
                                       (merge-stream (scale-stream S 3)
                                                     (scale-stream S 5)))))

(define S2 (cons-stream 1 (merge-stream (merge-stream (scale-stream S2 2)
                                                      (scale-stream S2 3))
                                        (scale-stream S2 5))))

;: Why is the value different between S and S2?
;: The merge-stream don't fit the associative law?
