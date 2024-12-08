#lang racket

(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))

(define (push stack x)
  (cons x stack)
  )

(define (push-list stack xs)
  (foldl (λ (x current-stack) (push current-stack x)) stack xs)
  )

(define (top stack)
  (car stack)
  )

(define (pop stack)
  (cdr stack)
  )

(define (enqueue queue x)
  (append queue (list x))
  )

(define (enqueue-list queue xs)
  (foldl (λ (x current-queue) (enqueue current-queue x)) queue xs)
  )

(define (peek queue)
  (car queue)
  )

(define (dequeue queue)
  (cdr queue)
  )

(define (contains? x xs)
  (list? (member x xs))
  )

(define (neighbours v graph)
  (cdr (list-ref graph (sub1 v)))
  )

(define (traverse start graph getFirstElementFn removeFirstElementFn addElementsFn)
  (define (helper container visited result)
    (cond
      [(empty? container) result]
      [(contains? (getFirstElementFn container) visited) (helper (removeFirstElementFn container) visited result)]
      [else (helper (addElementsFn (removeFirstElementFn container) (filter (λ (x) (not (contains? x visited))) (neighbours (getFirstElementFn container) graph)))
                    (cons (getFirstElementFn container) visited) (cons (getFirstElementFn container) visited))]
      )
    )
  (reverse (helper (list start) '() '()))
  )

(define (dfs start graph)
  (traverse start graph top pop push-list)
  )

(define (bfs start graph)
  (traverse start graph peek dequeue enqueue-list)
  )

(equal? (dfs 1 our-cool-graph) '(1 3 5 6 2 4))
(equal? (bfs 1 our-cool-graph) '(1 2 3 4 5 6))