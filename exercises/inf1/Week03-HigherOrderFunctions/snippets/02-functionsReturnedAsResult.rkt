#lang racket

(define (my-complex-function x f g)
    (if (> (f x) (g x)) f g)
)

(define (f1 x)
    (+ x 5)
)

(define (f2 x)
    (* x x)
)

(my-complex-function 3 f1 f2)