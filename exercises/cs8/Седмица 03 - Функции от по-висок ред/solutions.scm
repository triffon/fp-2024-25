(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (5+ x)
  (+ x 5))

(define (3* x)
  (* x 3))

(define 3+ (lambda (x)
             (* x 3)))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define (S f g)
  (lambda (x)
    (f x (g x))))

(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (repeat f n)
  (lambda (x)
    (if (zero? n) x
        (compose f (repeat f (- n 1))))))

(define (derive f)
  (let ((h 0.000001))
    (lambda (a)
      (/ (- (f (+ a h))
            (f a))
         h))))

(define (accumulate operation null-value begin end term next)
  (if (> begin end) null-value
      (operation (term begin)
                 (accumulate operation null-value (next begin) end term next))))

(define (accumulate-iter operation null-value begin end term next)                                                                       (define (accumulate operation null-value begin end term next)
  (if (> begin end) null-value
      (accumulate-iter operation (operation (term begin) null-value) (next begin) end term next))))

(define (1+ x)
  (+ x 1))
                                                                                
(define (sum-odd-squares a b)
  (accumulate-iter + 0 a b (lambda (x)
                        (if (odd? x)
                            (* x x) 0)) 1+))

(define (binomial n k)
  (accumulate * 1 1 k (lambda (i)
                        (/ (+ (- n i) 1) i)) 1+))

(define (id x) x)

(define (argmax f a b)
  (accumulate (lambda (current result)
                (if (> (f current)
                       (f result))
                    current
                    result))
              a (1+ a) b id 1+))

(define (&& a b)
  (and a b))

(define (all? predicate? a b)
  (accumulate && #t a b predicate? 1+))

(define (sum-exponents a b)
  (accumulate + 0 a b (lambda (c)
                        (accumulate + 0 a b ((curry expt) c) 1+)) 1+))

(define (const a)
  (lambda (x) a))

(define 1c (const 1))

(define (count-digits n)
  (accumulate + 0 1 n 1c (lambda (x) (* x 10))))
                        








  
