#lang racket

(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))

(define (remove-vertex v graph)
  (map (λ (vertex-and-neighbours) (cons (car vertex-and-neighbours)
                                        (filter (λ (neighbour) (not (= v neighbour))) (cdr vertex-and-neighbours))))
       (filter (λ (vertex-and-neighbours) (not (= v (car vertex-and-neighbours)))) graph))
)

(equal? (remove-vertex 5 our-cool-graph) '((1 2 3)
                                          (2 3)
                                          (3 4)
                                          (4)
                                          (6 2)))