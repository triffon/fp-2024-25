#lang racket

(define (leap-year-one-line? x)
  (or (= (remainder x 400) 0) (and (= (remainder x 4) 0) (not (= (remainder x 100) 0))))
  )

(define (leap-year-guards? x)
  (cond
    [(= (remainder x 400) 0) #t]
    [(= (remainder x 100) 0) #f]
    [(= (remainder x 4) 0) #t]
    [else #f]
    )
  )

(equal? (leap-year-one-line? 2000) #t)
(equal? (leap-year-one-line? 1900) #f)
(equal? (leap-year-one-line? 2004) #t)

(equal? (leap-year-guards? 2000) #t)
(equal? (leap-year-guards? 1900) #f)
(equal? (leap-year-guards? 2004) #t)