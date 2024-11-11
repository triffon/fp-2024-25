#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (filter pred? bt)
  (cond
    [(empty? bt) '()]
    [(pred? (root bt)) (cons (root bt) (append (filter pred? (left-subtree bt)) (filter pred? (right-subtree bt))))]
    [else (append (filter pred? (left-subtree bt)) (filter pred? (right-subtree bt)))]
    )
  )

(equal? (filter even? our-cool-tree) '(2 4))