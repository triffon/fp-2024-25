#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

; n! = 1 * 2 * 3 * ... * n

(define (factorial n)
  (accumulate * 1 1 n (λ (x) x) (λ (x) (add1 x)))
  )

(define (factorial1 n)
  (accumulate * 1 1 n identity add1)
  )

(= (factorial 5) 120)
(= (factorial 8) 40320)