#lang racket

(sort '(1 3 2) >)

(apply + '(1 2 3 4 5))

(map (λ (x) (* x 2)) '(1 2 3 4 5))
(map add1 '(1 2 3 4 5))

(filter even? '(1 2 3 4 5))
(filter (λ (x) (zero? (remainder x 3))) '(0 1 2 3 4 5 6))

(foldl (λ (x y) (* x y)) 1 '(1 2 3 4 5))
(foldr (λ (x y) (and x y)) #f '(#t #t #t #t #t))

(ormap even? '(1 2 3 4 5))

(andmap even? '(1 2 3 4 5))

(take '(1 2 3 4) 0)
(take '(1 2 3 4) 2)

(takef '(2 4 5 8) even?)
(takef '(2 4 6 8) odd?)

(drop '(1 2 3 4) 0)
(drop '(1 2 3 4) 2)

(dropf '(2 2 5 8 56) even?)
(dropf '(2 2 4 8 56) even?)