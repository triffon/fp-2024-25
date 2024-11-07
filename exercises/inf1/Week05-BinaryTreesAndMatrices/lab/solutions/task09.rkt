#lang racket

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (root bt) (car bt))
(define (left-subtree bt) (cadr bt))
(define (right-subtree bt) (caddr bt))

(define (map f bt)
  (if (empty? bt)
      '()
      (list (f (root bt)) (map f (left-subtree bt)) (map f (right-subtree bt)))
      )
  )

(equal? (map add1 our-cool-tree) '(2 (3 () ())
                                    (4 (5 () ())
                                        (6 () ()))))