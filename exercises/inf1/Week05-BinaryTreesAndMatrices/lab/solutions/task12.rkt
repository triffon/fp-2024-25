#lang racket

(define (transpose xss)
  (define (helper index)
    (map (λ (row) (list-ref row index)) xss)
    )
  (map helper (range 0 (length (car xss))))
  )

(transpose '((1 2 0)
             (3 4 1)
             (0 5 7)
             (4 2 4)))

(define (zero-rows xss)
  (map (λ (xs) (if (list? (member 0 xs)) (map (λ (x) 0) xs) xs)) xss)
  )

(define (zero-cols xss)
  (transpose (zero-rows (transpose xss)))
  )

(equal? (zero-cols '((1 2 0)
                     (3 4 1)
                     (0 5 7)
                     (4 2 4))) '((0 2 0)
                                 (0 4 0)
                                 (0 5 0)
                                 (0 2 0)))

