#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (product-of-digits n)
  (if (= n 0) 0 (accumulate * 1 1 n (λ (x) (remainder (quotient n x) 10)) (λ (x) (* x 10))))
  )

(= (product-of-digits 123) 6)
(= (product-of-digits 5) 5)
(= (product-of-digits 0) 0)
(= (product-of-digits 101) 0)
