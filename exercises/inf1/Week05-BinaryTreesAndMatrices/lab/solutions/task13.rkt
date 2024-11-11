#lang racket

(define (main-diagonal xss)
  (define (helper index)
    (list-ref (list-ref xss index) index)
    )
  (map helper (range 0 (min (length (car xss)) (length xss))))
  )

(define (secondary-diagonal xss)
  (define (helper row-index)
    (list-ref (list-ref xss row-index) (- (max (length (car xss)) (length xss)) (add1 row-index)))
    )
  (map helper (range 0 (min (length (car xss)) (length xss))))
  )

(main-diagonal '((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(secondary-diagonal '((1 2 3)
                 (4 5 6)
                 (7 8 9)))

; [0, 0] [1, 1] [2, 2]
; [0, 2] [1, 1] [2, 0]

(define (diagonals xss) (cons (main-diagonal xss) (secondary-diagonal xss)))

(diagonals '((1  2  3  4)
             (5  6  7  8)
             (9 10 11 12)))

(equal? (diagonals '((1 2 3)
                     (4 5 6)
                     (7 8 9))) '((1 5 9) . (3 5 7)))
(equal? (diagonals '((10 20)
                     (30 40))) '((10 40) . (20 30)))
(equal? (diagonals '((1  2  3  4)
                     (5  6  7  8)
                     (9 10 11 12))) '((1 6 11) . (4 7 10)))
(equal? (diagonals '((100))) '((100) . (100)))

