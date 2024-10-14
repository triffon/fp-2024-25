#lang racket

(define (help-preslava hour-beginning minutes-beginning songs-count)
  (define beginning-in-minutes (+ (* 60 hour-beginning) minutes-beginning))
  (define ending-in-minutes (+ beginning-in-minutes (* songs-count 3)))
  (cons (quotient ending-in-minutes 60) (remainder ending-in-minutes 60))
  )

(equal? (help-preslava 2 0 10) '(2 . 30))
(equal? (help-preslava 2 30 15) '(3 . 15))