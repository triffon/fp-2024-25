#lang racket

(define (count-occurrences xs ys)
  (define (helper current result)
    (cond
      [(< (length current) (length xs)) result]
      [(equal? (take current (length xs)) xs) (helper (cdr current) (add1 result))]
      [else (helper (cdr current) result)]
      )
    )
  (helper ys 0)
  )

(= (count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurrences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurrences '(1 1 1) '(1 1 1 1 1 1 1 1)) 6)
(= (count-occurrences '(6 6) '(2 2)) 0)