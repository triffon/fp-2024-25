#lang racket

(define (ordered? xs)
  (define (helper current condition result)
    (if (< (length current) 2)
        result
        (helper (cdr current) condition (and (condition (car current) (cadr current)) result))
        )
    )
  (λ (pred?) (helper xs pred? #t))
  )

(equal? ((ordered? '(1 2 3 5)) (λ (x y) (< x y))) #t)
(equal? ((ordered? '(1 8 29 92)) (λ (x y) (= y (+ (* x 3) 5)))) #t)
(equal? ((ordered? '(1 8 3 14)) (λ (x y) (= y (+ (* x 3) 5)))) #f)