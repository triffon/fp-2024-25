#lang racket

(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))

(define (path? u v graph)
  (define (helper current-start visited)
    (let (
        [neighbours (cdr (list-ref graph (sub1 current-start)))]
        )
    (cond
      [(or (empty? neighbours) (list? (member current-start visited))) #f]
      [(list? (member v neighbours)) #t]
      [else (ormap (λ (neigbour) (helper neigbour (cons current-start visited))) neighbours)]
      )
    )
  )
  (helper u '())
)

(equal? (path? 1 2 our-cool-graph) #t)
(equal? (path? 4 2 our-cool-graph) #f)
(equal? (path? 1 6 our-cool-graph) #t)
(equal? (path? 5 1 our-cool-graph) #f)