(define a 4)
(define b -2)

(+ a b)

(define a 8)

(define ^ expt)
(define % remainder)

;((12 + 8) / (4 - sqrt(4)))^3

(^ (/ (+ 12 8)
      (- 4
         (sqrt 4)))
   3)


(define здр 3)
(define 5+ 8)

(define (divisible-by? d n)
  (zero? (remainder d n)))

(define (sign n)
  (cond ((< n 0) "negative")
        ((> n 0) "positive")
        (else "zero")))

(define (distance x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2)
           (expt (- y1 y2) 2))))

(define (middle a b c)
  (- (+ a b c)
     (max a b c)
     (min a b c)))

(define (super-number a b c)
  (+ (* (max a b c)
        (min a b c))
     (middle a b c)))

(define (reverse-3-digit-number n)
  (+ (* 100 (remainder n 10))
     (* 10 (remainder (quotient n 10) 10))
     (quotient n 100)))


(define (leap? year)
  (or (and (divisible-by? year 4)
           (not (divisible-by? year 100)))
      (divisible-by? year 400)))

(define (factorial n)
  (if (zero? n) 1
      (* n (factorial (- n 1)))))

(define (interval-sum begin end)
  (if (= begin end) end
      (+ begin (interval-sum (+ begin 1) end))))

(define (length-number n)
  (if (< n 10) 1
      (+ 1 (length-number (quotient n 10)))))

(define (reverse-number n)
  (if (< n 10) n
      (+ (* (remainder n 10)
            (expt 10 (- (length-number n) 1)))
         (reverse-number (quotient n 10)))))





      








