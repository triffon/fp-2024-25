(define (factorial n)
  (if (zero? n) 1
      (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (for i result)
    (if (zero? i) result
        (for (- i 1)
             (* result i))))
  (for n 1))

(define (func n)
  (let ((a (+ n 5))
        (b (* n 2)))
    (- a b)))

(define (divisors-sum n)
  (define (for i result)
    (if (= i n)
        (+ result i)
        (for (+ i 1)
             (if (zero? (remainder n i))
                 (+ result i)
                 result))))
  (for 1 0))

(define (reverse-number n)
  (define (while current result)
    (if (zero? current) result
        (while (quotient current 10)
               (+ (* result 10)
                  (remainder current 10)))))
  (while n 0))

(define (decimal-to-binary n)
  (define (while current result degree)
    (if (zero? current) result
        (while (quotient current 2)
               (+ result
                  (* (remainder current 2) (expt 10 degree)))
               (+ degree 1))))
  (while n 0 0))

(define (fibonacci n)
  (if (or (= n 1)
          (= n 2))
      1
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (fibonacci-iter n)
  (define (for i a b)
    (if (= i n)
        (+ a b)
        (for (+ i 1) b (+ a b))))
  (if (or (= n 1)
          (= n 2)) 1
      (for 3 1 1)))

(define (fast-pow x n)
  (define (while base degree result)
    (cond ((zero? degree) result)
          ((even? degree)
           (while (* base base)
                  (quotient degree 2)
                  result))
          (else (while base (- degree 1) (* result base)))))
  (while x n 1))
                      









