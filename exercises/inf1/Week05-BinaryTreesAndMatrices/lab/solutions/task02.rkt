#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (size bt)
  (if (empty? bt)
      0
      (+ 1 (size (left-subtree bt)) (size (right-subtree bt))
      )
    )
)

(= (size our-cool-tree) 5)