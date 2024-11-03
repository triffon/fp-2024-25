#lang racket

(define (get-ascending-sublist-from start-index xs)
  (define (helper current-index result)
    (if (or (= (add1 current-index) (length xs)) (> (list-ref xs current-index) (list-ref xs (add1 current-index))))
        result
        (helper (add1 current-index) (cons (list-ref xs (add1 current-index)) result))
        )
    )
  (if (>= (add1 start-index) (length xs))
      '()
      (cons (list-ref xs start-index) (reverse (helper start-index '())))
      )
  )

(define (longest-ascending-sub xs)
  (define (helper current-sublist current-index max-len best-sublist)
    (cond
      [(empty? current-sublist) best-sublist]
      [(> (length current-sublist) max-len) (helper (get-ascending-sublist-from (add1 current-index) xs) (add1 current-index) (length current-sublist) current-sublist)]
      [else (helper (get-ascending-sublist-from (add1 current-index) xs) (add1 current-index) max-len best-sublist)]
      )
    )
  (helper (get-ascending-sublist-from 0 xs) 0 0 '())
  )

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))