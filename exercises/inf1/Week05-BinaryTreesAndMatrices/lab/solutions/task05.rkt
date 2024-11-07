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

(define (perfectly-balanced? bt)
  (or
   (empty? bt)
   (and
    (< (abs (- (size (left-subtree bt)) (size (right-subtree bt)))) 2)
    (perfectly-balanced? (left-subtree bt))
    (perfectly-balanced? (right-subtree bt)))
   )
  )

(equal? (perfectly-balanced? our-cool-tree) #f)