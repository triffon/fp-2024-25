(define fact (lambda (n)
               (if (= n 0) 1 (* n (fact (- n 1))))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/ (accumulate * 1 1 i (lambda (i) x) 1+)
                                     (accumulate * 1 1 i (lambda (i) i) 1+))) 1+))

(define (exists? a b p?)
  (accumulate (lambda (x y) (or x y)) #f a b p? 1+))

(define (square x) (* x x))
(define (twice f x) (f (f x)))
(define (twice f) (lambda (x) (f (f x))))

(define (n+ n)
  (lambda (i) (+ i n)))

(define 1+ (n+ 1))
(define 5+ (n+ 5))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))