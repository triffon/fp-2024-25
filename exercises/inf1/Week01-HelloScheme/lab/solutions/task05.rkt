#lang racket

(define (cube-pow n) (expt n 3))
(define (sum-cubes-pow a b) (+ (cube-pow a) (cube-pow b)))

(define (cube-no-pow n) (* n n n))
(define (sum-cubes-no-pow a b) (+ (cube-no-pow a) (cube-no-pow b)))

(= (sum-cubes-pow 5 1) 126)
(= (sum-cubes-pow 10 50) 126000)

(= (sum-cubes-no-pow 5 1) 126)
(= (sum-cubes-no-pow 10 50) 126000)