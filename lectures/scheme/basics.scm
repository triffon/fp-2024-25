(define (square x) (* x x))
(define (1+ k) (+ k 1))
(define (g x) (- (g (+ x 1)) 1))
(define (f x) (cond ((< x 3) (+ x 1))))
(square 5)

(define (fact n)
  (define (fact n) (* n 10))
  (if (= n 0) 1
      (* n (fact (- n 1)))))
