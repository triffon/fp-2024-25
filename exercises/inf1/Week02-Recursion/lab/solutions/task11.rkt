#lang racket

(define (count-of-digits n)
  (define (helper counter current)
    (if (zero? current)
        counter
        (helper (add1 counter) (quotient current 10))
        )
    )
  (helper 0 n)
)

(define (get-last-n-digits-of number n)
  (remainder number (expt 10 n))
  )

(define (subnumber? x y)
  (define (helper current)
    (cond
      [(> x current) #f]
      [(= x (get-last-n-digits-of current (count-of-digits x))) #t]
      [else (helper (quotient current 10))]
      )
    )
  (helper y)
  )

(equal? (subnumber? 123 5123783) #t)
(equal? (subnumber? 0 0) #t)
(equal? (subnumber? 10 101) #t)
(equal? (subnumber? 101 101) #t)
(equal? (subnumber? 10 0) #f)
(equal? (subnumber? 1253 5123783) #f)
(equal? (subnumber? 12 0) #f)