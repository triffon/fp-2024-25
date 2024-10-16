(define (square x) (* x x))
(define (1+ k) (+ k 1))
(define (g x) (- (g (+ x 1)) 1))
(define (f x) (cond ((< x 3) (+ x 1))))
(square 5)

(define (fact n)
  (define (fact n) (* n 10))
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        (else (* x (pow x (- n 1))))))

(define (qpow x n)
  (define (sq x) (* x x))
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (qpow x (- n))))
        ((even? n) (sq (qpow x (quotient n 2))))
        (else (* x (qpow x (- n 1))))))

;; (qpow 2 2) --> (qpow (qpow 2 1) 2) --> (qpow 2 2) --> ....


(define (fib n)
  (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))