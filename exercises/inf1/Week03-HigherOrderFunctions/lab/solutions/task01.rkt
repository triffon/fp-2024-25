#lang racket

(define (my-identity)
  (λ (x) x)
  )

(define (my-lambda f)
  (λ (x) (f x))
  )

(= ((my-identity) 7) 7)
(equal? ((my-identity) "FMI") "FMI")

(= ((my-lambda identity) 7) 7)
(equal? ((my-lambda identity) "FMI") "FMI")
(= ((my-lambda string-length) "FMI") 3)

(define (negate-pred pred?)
  (λ (x) (not (pred? x)))
  )

(equal? ((negate-pred even?) 6) #f)

(define (my-compose f g)
  (λ (x) (f (g x)))
  )

(equal? ((my-compose even? string-length) "Tensorflow") #t)
(equal? ((my-compose (λ (x) (- x 5)) (λ (y) (+ y 25))) 5) 25)