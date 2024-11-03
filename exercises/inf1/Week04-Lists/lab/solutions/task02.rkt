#land racket

(define (my-length-rec xs)
  (if (equal? xs '()) 0 (add1 (my-length-rec (cdr xs))))
  )

(define (my-length-apply xs)
  (apply + (map (λ (x) 1) xs))
  )

(define (my-length-fold xs)
  (foldl (λ (x acc) (add1 acc)) 0 xs)
  )

(= (my-length-rec '()) 0)
(= (my-length-rec '(1 2 5 6 4 8)) 6)

(= (my-length-apply '()) 0)
(= (my-length-apply '(1 2 5 6 4 8)) 6)

(= (my-length-fold '()) 0)
(= (my-length-fold '(1 2 5 6 4 8)) 6)