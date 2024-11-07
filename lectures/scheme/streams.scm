(define the-empty-stream '())
(define empty-stream? null?)
;; (define (cons-stream h t) (cons h (delay t)))
(define-syntax cons-stream
   (syntax-rules () ((cons-stream h t) (cons h (delay t)))))
(define head car)
(define (tail s) (force (cdr s)))

(define-syntax delay
  (syntax-rules () ((delay x) (lambda () x))))

(define (force x) (x))

(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 3 (cons-stream b the-empty-stream)))

(define (enum a b)
  (if (> a b) the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (take n s)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons (head s) (take (- n 1) (tail s)))))

(define (find-stream p? s)
  (cond ((empty-stream? s) #f)
        ((p? (head s)) s)
        (else (find-stream p? (tail s)))))

(define (from n)
  (cons-stream n (from (+ n 1))))

(define nats (from 0))

(define (generate-fibs a b)
  (cons-stream a (generate-fibs b (+ a b))))

(define fibs (generate-fibs 0 1))

(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))

(define (filter-stream p? s)
  (if (p? (head s)) (cons-stream (head s) (filter-stream p? (tail s)))
                    (filter-stream p? (tail s))))

(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-streams op (tail s1) (tail s2))))

(define (1+ x) (+ x 1))

(define ones (cons-stream 1 ones))
(define nats (cons-stream 0 (zip-streams + ones nats)))
(define nats (cons-stream 0 (map-stream 1+ nats)))
(define fibs
  (cons-stream 0 (cons-stream 1 (zip-streams + fibs (tail fibs)))))

(define (nondivisor d) (lambda (n) (> (remainder n d) 0)))

(define (sieve stream)
  (cons-stream (head stream) (sieve (filter-stream (nondivisor (head stream)) (tail stream)))))

(define primes (sieve (from 2)))

