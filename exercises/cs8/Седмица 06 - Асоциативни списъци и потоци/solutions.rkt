#lang racket

(define (make-assoc f l)
  (map (lambda (x)
         (cons x (f x))) l))

(define assoc-list (make-assoc (lambda (x) (* x x)) '(1 2 3 4 5)))

(define (keys al)
  (map car al))

(define (values* al)
  (map cdr al))


(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))

(define (filter predicate? lst)
  (foldr (lambda (current result)
           (if (predicate? current)
               (cons current result)
               result)) '() lst))

(define (search key al)
  (foldr (lambda (current result)
           (if (equal? (car current) key)
               current result)) #f al))

(define (add-assoc key value al)
  (cons (cons key value)
        (filter (lambda (x)
            (not (equal? (car x) key))) al)))

(define (add-assoc-ord key value al)
  (let ([pair (search key al)])
    (if [not pair]
        (cons (cons key value) al)
        (foldr (λ (current result)
             (if (equal? (car current) key)
                 (cons (cons key value) result)
                 (cons current result)))
           '() al))))

(define (unique lst)
  (foldr (lambda (current result)
           (if (member current result)
               result
               (cons current result)))
         '() lst))

(define (histogram lst)
  (define (count x lst)
    (length (filter (lambda (y) (equal? x y)) lst)))
  (unique (make-assoc (lambda (x) (count x lst)) lst)))


(define (histogram-a lst)
  (foldr (λ (current result)
           (let ([pair (search current result)])
             (if (not pair)
                 (add-assoc current 1 result)
                 (add-assoc current (+ 1 (cdr pair)) result))))
         '() lst))

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s))) 

(define (take n stream)
  (if (or (zero? n)
          (null? stream)) '()
      (cons (head stream)
            (take (- n 1) (tail stream)))))

(define (from n)
  (cons-stream n (from (+ n 1))))

(define nats (from 0))

(define (map-stream f stream)
  (if (null? stream) '()
      (cons-stream (f (head stream))
                   (map-stream f (tail stream)))))

(define nats-m
  (cons-stream 0 (map-stream (lambda (x) (+ x 1)) nats-m)))

(define (fibs-helper a b)
    (cons-stream a (fibs-helper b (+ a b))))

(define fibs (fibs-helper 0 1))

(define (zip-with-stream f stream1 stream2)
  (if (or (null? stream1)
          (null? stream2))
      '()
      (cons-stream (f (head stream1)
                      (head stream2))
                   (zip-with-stream f
                                    (tail stream1)
                                    (tail stream2)))))

(define fibs-m
  (cons-stream 0
               (cons-stream 1
                            (zip-with-stream + fibs-m (tail fibs-m)))))

(define (zip lst1 lst2)
  (if (or (null? lst1)
          (null? lst2)) '()
      (cons (cons (car lst1)
                  (car lst2))
            (zip (cdr lst1)
                 (cdr lst2)))))

(define (tupled f)
  (lambda (x)
    (f (car x)
       (cdr x))))

(define (zip-with f lst1 lst2)
  (map (tupled f) (zip lst1 lst2)))

(define (next-pascal-row row)
  (zip-with + (cons 0 row) (append row (list 0))))

(define pascal-triangle
  (cons-stream '(1) (map-stream next-pascal-row pascal-triangle)))




  
  