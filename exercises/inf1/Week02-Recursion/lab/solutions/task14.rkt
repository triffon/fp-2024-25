#lang racket

(define (max-digit-in-number n)
  (if (< n 10)
      n
      (max (remainder n 10) (max-digit-in-number (quotient n 10)))
      )
  )

(define (remove-first-occurrence n d)
  (cond
    [(= n d) 0]
    [(< n 10) n]
    [(= (remainder n 10) d) (quotient n 10)]
    [else (+ (* 10 (remove-first-occurrence (quotient n 10) d)) (remainder n 10))]
    )
  )

(define (rev n)
  (define (helper result current)
    (if (zero? current)
        result
        (helper (+ (* result 10) (remainder current 10)) (quotient current 10))
        )
    )
  (helper 0 n)
  )

(define (sort-n n)
  (define (helper current result)
    (cond
      [(< current 10) (+ (* result 10) current)]
      [else (let (
                   [max-digit (max-digit-in-number current)]
                   ) (helper (remove-first-occurrence current max-digit) (+ (* result 10) max-digit)))]
      )
    )
  (helper n 0)
  )

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)