#lang racket

(define (insert-at x position xs)
  (append (take xs (sub1 position)) (cons x (drop xs (sub1 position))))
  ;(append (take xs (sub1 position)) (list x) (drop xs (sub1 position)))
  )

(equal? (insert-at 0 1 '(10 20 30)) '(0 10 20 30))
(equal? (insert-at 0 2 '(10 20 30)) '(10 0 20 30))
(equal? (insert-at 0 3 '(10 20 30)) '(10 20 0 30))
(equal? (insert-at 0 4 '(10 20 30)) '(10 20 30 0))
(equal? (insert-at 0 1 '()) '(0))