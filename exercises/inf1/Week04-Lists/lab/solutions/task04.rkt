#lang racket

(define (remove-first x xs)
  (cond
    [(empty? xs) xs]
    [(equal? (car xs) x) (cdr xs)]
    [else (cons (car xs) (remove-first x (cdr xs)))]
    )
  )

(equal? (remove-first 1 '(1 1 1 2)) '(1 1 2))
(equal? (remove-first 1 '(2 5 6)) '(2 5 6))
(equal? (remove-first 1 '(1)) '())
(equal? (remove-first 1 '(2 1)) '(2))
(equal? (remove-first "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN" "RNN"))

(define (remove-all x xs)
  (cond
    [(empty? xs) xs]
    [(equal? (car xs) x) (remove-all x (cdr xs))]
    [else (cons (car xs) (remove-all x (cdr xs)))]
    )
  )

(equal? (remove-all 1 '(1 1 1 2)) '(2))
(equal? (remove-all 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all 1 '(1)) '())
(equal? (remove-all 1 '(1 2 1 1)) '(2))
(equal? (remove-all "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))