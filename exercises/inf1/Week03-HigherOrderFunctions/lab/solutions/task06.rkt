#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (prime? n)
  (if (= n 1)
      #f
      (accumulate (λ (x y) (and x y)) #t 2 (floor (sqrt n)) (λ (x) (not (zero? (remainder n x)))) add1)
  ))

(equal? (prime? 1) #f)
(equal? (prime? 2) #t)
(equal? (prime? 3) #t)
(equal? (prime? 6) #f)
(equal? (prime? 42) #f)
(equal? (prime? 61) #t)
