#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (count-digits n)
  (if (zero? n) 1 (accumulate + 0 1 n (λ (x) 1) (λ (x) (* x 10))))
)

(= (count-digits 12345) 5)
(= (count-digits 5) 1)
(= (count-digits 55) 2)
(= (count-digits 555) 3)
(= (count-digits 0) 1)