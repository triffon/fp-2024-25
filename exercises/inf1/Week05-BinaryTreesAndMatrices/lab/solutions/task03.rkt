#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (height bt)
  (if (empty? bt)
      0
      (add1 (max (height (left-subtree bt)) (height (right-subtree bt))))
      )
  )

(= (height our-cool-tree) 3)