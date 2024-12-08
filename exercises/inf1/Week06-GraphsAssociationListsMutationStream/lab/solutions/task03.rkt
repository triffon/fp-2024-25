#lang racket

(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))

(define (filter-children graph pred? v)
  (filter pred? (cdr (list-ref graph (sub1 v))))
)

(equal? (filter-children our-cool-graph even? 3) '(4))