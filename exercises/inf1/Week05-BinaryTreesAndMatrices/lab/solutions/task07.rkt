#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (leaf? bt)
  (and (not (empty? bt)) (empty? (left-subtree bt)) (empty? (right-subtree bt)))
  )

(define (leaves bt)
  (cond
    [(empty? bt) '()]
    [(leaf? bt) (list (root bt))]
    [else (append (leaves (left-subtree bt)) (leaves (right-subtree bt)))]
    )
  )

(equal? (leaves our-cool-tree) '(2 4 5))