#lang racket

;; Useful helper functions
(define (last-digit n) (remainder n 10))
(define (first-digits n) (quotient n 10))
(define (divides i n) (= 0 (remainder n i)))
(define (++ i) (+ 1 i))


;; 1
(define (sum-digits n)
  (if (= n 0)
      0
      (+ (last-digit n) (sum-digits (first-digits n)))))

(define (sum-digits-tailrec n)
  (define (helper n acc)
    (if (= n 0)
        acc
        (helper (first-digits n) (+ acc (last-digit n)))))

  (helper n 0))


;; 2
(define (count-divisors n)
  (define (helper i acc)
    (cond
      ((> (* i i) n) acc)
      ((= (* i i) n) (+ acc 1))
      ((divides i n) (helper (++ i) (+ acc 2)))
      (else (helper (++ i) acc))))

  (helper 1 0))


;; 3
(define (prime? n)
  (= (count-divisors n) 2))


;; 4
(define (increasing-digits? n)
  (define (2nd-last-digit n) (last-digit (first-digits n)))

  (if (< n 10)
      #t
      (and (< (2nd-last-digit n) (last-digit n)) (increasing-digits? (first-digits n)))))


;; 5.0
(define (ends-with? n k)
  (if (< k 10)
      (= (last-digit n) k)
      (and (= (last-digit n) (last-digit k))
           (ends-with? (first-digits n) (first-digits k)))))


;; 5.1
(define (automorphic? n)
  (ends-with? (* n n) n))


;; 6
(define (perfect? n)
  (define (sum-divisors i acc)
    (if (= i n)
        acc
        (sum-divisors (++ i) (+ acc (if (divides i n) i 0)))))

  (= n (sum-divisors 1 0)))


;; 7 + 8
(define (change-base n base-from base-to)
  (define (helper n pow acc)
    (if (= n 0)
        acc
        (helper (quotient n base-to) (* pow base-from) (+ acc (* pow (remainder n base-to))))))

  (helper n 1 0))

(define (binary-to-decimal n)
  (change-base n 2 10))

(define (decimal-to-binary n)
  (change-base n 10 2))
