#lang racket

(define (replace xs dict)
  (map (λ (x) (if (pair? (assoc x dict)) (cdr (assoc x dict)) x)) xs)
  )

(equal? (replace '(1 2 3 4) '((1 . 11) (2 . 22))) '(11 22 3 4))