#lang racket

(define (increasing-digits? n)
  (cond
    [(< n 10) #t]
    [(< (remainder n 10) (remainder (quotient n 10) 10)) #f]
    [else (increasing-digits? (quotient n 10))]
    )
  )

(equal? (increasing-digits? 1244) #t)
(equal? (increasing-digits? 12443) #f)