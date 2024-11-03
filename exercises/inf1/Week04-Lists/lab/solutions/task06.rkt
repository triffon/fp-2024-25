#lang racket

(define (set-union first-list second-list)
   (sort (remove-duplicates (append first-list second-list)) <)
  )

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))