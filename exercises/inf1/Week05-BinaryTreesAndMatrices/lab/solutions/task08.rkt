#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (at-level level bt)
  (cond
    [(empty? bt) '()]
    [(zero? level) (list (root bt))]
    [else (append (at-level (sub1 level) (left-subtree bt)) (at-level (sub1 level) (right-subtree bt)))]
    )
  )

(equal? (at-level 0 our-cool-tree) '(1))
(equal? (at-level 1 our-cool-tree) '(2 3))
(equal? (at-level 2 our-cool-tree) '(4 5))