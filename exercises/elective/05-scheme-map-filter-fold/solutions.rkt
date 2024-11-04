#lang racket

;; ВАЖНО!
;; Ще използваме foldl от стандартната библиотека, в която бинарната операция
;; взима настоящия елемент и акумулатора в същия ред като foldr, т.е. наобратно
;; от имплементацията от лекции


;; Помощни функции
(define (const c) (lambda (x) c))

(define (|| x y) (or x y))
(define (&& x y) (and x y))


;; 1
;; Няма значение дали ползваме `foldl` или `foldr`, защото `+` е комутативен
(define (sum l)
  (foldr + 0 l))

;; 2
;; Отново няма значение дали ползваме `foldl` или `foldr`
(define (len xs)
  (foldr (lambda (x acc) (+ 1 acc)) 0 xs))

;; Вариант със `map` и `sum` от зад. 1
(define (len-via-sum xs)
  (sum (map (const 1) xs)))

;; 3
(define (any? p xs)
  (foldr || #f (map p xs)))

(define (all? p xs)
  (foldr && #t (map p xs)))

;; 4
(define (foldl1 f xs)
  (foldl f (car xs) (cdr xs)))

(define (last xs)
  (car (reverse xs)))

(define (all-but-last xs)
 (reverse (cdr (reverse xs))))

(define (foldr1 f xs)
  (foldr f (last xs) (all-but-last xs)))

;; 5
;; Можем да използваме и `foldr1`
(define (minimum xs)
  (foldl1 min xs))

(define (maximum xs)
  (foldl1 max xs))

;; 6
;; Тук вече **трябва** да използваме `foldr`, тъй като `cons` **Не** е комутативенс
(define (map f xs)
  (foldr (lambda (x acc) (cons (f x) acc)) (list) xs))

;; 7
(define (filter p xs)
  (foldr (lambda (x acc) (if (p x) (cons x acc) acc)) (list) xs))

;; 8
;; Какво ще се случи, ако вместо `foldl` използваме `foldr`?
(define (reverse xs)
  (foldl cons '() xs))

;; 9
(define (take n xs)
  (cond
    ((<= n 0) (list))
    ((null? xs) (list))
    (else (cons (car xs) (take (- n 1) (cdr xs))))))

;; `take` и `drop` са "огледални" функции
(define (drop n xs)
  (cond
    ((<= n 0) xs)
    ((null? xs) xs)
    (else (drop (- n 1) (cdr xs)))))


;; 10
(define (take-while p? xs)
  (cond
    ((null? xs) xs)
    ((not (p? (car xs))) (list))
    (else (cons (car xs) (take-while p? (cdr xs))))))

;; `take-while` и `drop-while` са "огледални" функции
(define (drop-while p? xs)
  (cond
    ((null? xs) xs)
    ((p? (car xs)) xs)
    (else (drop-while p? (cdr xs)))))

;; 11
(define (zip-with f xs ys)
  (cond
    ((or (null? xs) (null? ys)) (list))
    (else (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys))))))

;; Може да дефинираме `zip` чрез `zip-with`
(define (zip xs ys)
  (zip-with cons xs ys))
