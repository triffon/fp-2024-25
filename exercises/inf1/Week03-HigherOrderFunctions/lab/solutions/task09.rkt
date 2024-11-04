#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (argmin f a b)
  (accumulate (λ (x y) (if (< (f x) (f y)) x y)) a (add1 a) b identity add1)
  )

(define (mod7 x) (remainder x 7))
(argmin mod7 45 50)
(= (argmin mod7 45 50) 49)