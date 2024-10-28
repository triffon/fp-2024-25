#lang racket

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (any? a b pred?)
  (accumulate (λ (x y) (or x y)) #f a b pred? add1)
  )

(define (count-pairs a b n)
  (accumulate + 0 a b (λ (x) (if (any? x b (λ (y) (= (+ x y) n))) 1 0)) add1)
  )

(= (count-pairs 1 10 14) 4)