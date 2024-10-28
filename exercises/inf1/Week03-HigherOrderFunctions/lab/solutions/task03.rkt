#lang racket

(define (apply-n f n)
  (define (helper left-over result)
    (if (> left-over n)
        result
        (helper (add1 left-over) (f result)))
    )
  (λ (x) (helper 1 x))
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)