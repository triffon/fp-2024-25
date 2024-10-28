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

(define (contains-digit? n d)
  (define (helper current)
    (cond
      [(= current 0) #f]
      [(= d (remainder current 10)) #t]
      [else (helper (quotient current 10))]
      )
    )
  (helper n)
  )

(define (sum-first-special-numbers n d)
  (define (helper result current-count current-number)
    (cond
      [(= current-count n) result]
      [(and (prime? current-number) (contains-digit? current-number d))
       (helper (+ result current-number) (add1 current-count) (add1 current-number))]
      [else (helper result current-count (add1 current-number))]
      )
    )
  (helper 0 0 2)
  )

(= (sum-first-special-numbers 5 2) 392)
(= (sum-first-special-numbers 5 3) 107)
(= (sum-first-special-numbers 10 3) 462)