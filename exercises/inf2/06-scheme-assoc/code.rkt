#lang racket

(define ages
  (list
   (cons 'pesho 28)
   (cons 'gosho 67)
   (cons 79 20)
   (cons 'ivan #f)))

(define friends
  (list
   (cons 'pesho (list 'gosho 'ivanka))
   (cons 'gosho (list 'ivanka))
   (cons 'penka (list))))

(define (exists? p l)
  (if (null? l)
      #f
      (or (p (car l))
          (exists? p (cdr l)))))

(define assoc-empty
  '())

(define (assoc-empty? l)
  (null? l))

(define (assoc-set l k v)
  (define (update-kv pair)
    (if (equal? (car pair) k)
        (cons k v)
        pair))
    
  (if (exists? (lambda (pair) (equal? (car pair) k)) l)
      (map (lambda (pair) (update-kv pair)) l)
      (cons (cons k v) l)))
  