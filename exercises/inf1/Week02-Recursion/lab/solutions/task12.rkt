#lang racket

(define (sum-of-digits n)
  (if (< n 10)
      n
      (+ (remainder n 10) (sum-of-digits (quotient n 10)))
      )
  )

(define (digital-root n)
  (cond
    [(< n 10) n]
    [else (digital-root (sum-of-digits n))]
    )
  )

(= (digital-root 5) 5)
(= (digital-root 39) 3)
(= (digital-root 493193) 2)