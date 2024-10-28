#lang racket

(define (prime? n)
  (define (helper current)
    (cond
      [(> current (sqrt n)) #t]
      [(zero? (remainder n current)) #f]
      [else (helper (add1 current))]
      )
    )
  (and (not (= n 1)) (helper 2))
  )

(equal? (prime? 1) #f)
(equal? (prime? 2) #t)
(equal? (prime? 3) #t)
(equal? (prime? 6) #f)
(equal? (prime? 42) #f)
(equal? (prime? 61) #t)