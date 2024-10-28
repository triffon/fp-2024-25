#lang racket

(define (rev n)
  (define (helper result current)
    (if (zero? current)
        result
        (helper (+ (* result 10) (remainder current 10)) (quotient current 10))
        )
    )
  (helper 0 n)
  )

(define (palindrome? n)
  (= n (rev n))
  )

(define (palindromes-between a b)
  (define (helper current result)
    (cond
      [(> current b) result]
      [(palindrome? current) (helper (add1 current) (add1 result))]
      [else (helper (add1 current) result)]
      )
    )
  (if (> a b) (palindromes-between b a) (helper a 0))
  )

(= (palindromes-between 1 101) 19)
(= (palindromes-between 1 100) 18)
(= (palindromes-between 100 1) 18)