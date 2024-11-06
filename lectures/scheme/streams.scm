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



(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 3 (cons-stream b the-empty-stream)))