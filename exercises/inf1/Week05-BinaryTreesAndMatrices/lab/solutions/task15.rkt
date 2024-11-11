#lang racket

(require racket/trace)

(define (at row column xss)
  (list-ref (list-ref xss row) column)
  )

(define (rows xss) (length xss))

(define (columns xss) (length (car xss)))

(define (size xss) (* (rows xss) (columns xss)))

(define (spiral xss)
  (define (helper up down left right result)
    (cond
      [(= (size xss) (length result)) result]
      [else (let* (
                   [left-right (map (λ (i) (at up i xss)) (range left (add1 right)))]
                   [up-down (map (λ (i) (at i right xss)) (range (add1 up) (add1 down)))]
                   [right-left (map (λ (i) (at down i xss)) (range (sub1 right) (sub1 left) -1))]
                   [down-up (map (λ (i) (at i left xss)) (range (sub1 down) up -1))]
                   )
              (helper (add1 up) (sub1 down) (add1 left) (sub1 right) (append result left-right up-down right-left down-up))
              )]
      )
    )
  (helper 0 (sub1 (rows xss)) 0 (sub1 (columns xss)) '())
  )

(equal? (spiral '((1 2 3)
                  (8 9 4)
                  (7 6 5))) '(1 2 3 4 5 6 7 8 9))