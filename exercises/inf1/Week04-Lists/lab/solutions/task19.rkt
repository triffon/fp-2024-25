#lang racket

(define (shuffle xs)
  (define (helper ys zs result)
    (if (empty? ys) result (helper (cdr ys) (cdr zs) (append result (list (car ys) (car zs)))))
    )

  (helper (take xs (quotient (length xs) 2)) (drop xs (quotient (length xs) 2)) '())
  )

(equal? (shuffle '(2 5 1 3 4 7)) '(2 3 5 4 1 7))
(equal? (shuffle '(1 2 3 4 4 3 2 1)) '(1 4 2 3 3 2 4 1))
(equal? (shuffle '(1 1 2 2)) '(1 2 1 2))