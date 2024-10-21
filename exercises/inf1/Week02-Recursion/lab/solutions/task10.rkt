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

(define (automorphic? n)
  (= n (get-last-n-digits-of (* n n) (count-of-digits n)))
  )

(equal? (automorphic? 3) #f)
(equal? (automorphic? 10) #f)
(equal? (automorphic? 5) #t)
(equal? (automorphic? 25) #t)
(equal? (automorphic? 76) #t) 
(equal? (automorphic? 890625) #t) 
(equal? (automorphic? 625) #t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)