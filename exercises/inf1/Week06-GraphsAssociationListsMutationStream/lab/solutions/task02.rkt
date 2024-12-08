#lang racket

(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))

(define (edges graph)
  (define (helper vertex-and-neighbours)
    (map (λ (neighbour) (cons (car vertex-and-neighbours) neighbour)) (cdr vertex-and-neighbours))
    )
  (foldl (λ (current acc) (append acc (helper current))) '() graph)
)

(equal? (edges our-cool-graph) '((1 . 2) (1 . 3) (2 . 3) (3 . 4) (3 . 5) (5 . 2) (5 . 4) (5 . 6) (6 . 2)))