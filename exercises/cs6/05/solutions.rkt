#lang racket

(define (foldr* op acc lst)
  (if (null? lst)
      acc
      (op (car lst)
          (foldr* op acc (cdr lst)))))

;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

; Използвайте apply където можете
;
; HINT: Може да използвате наготово всички функции писани до момента

; 1. Сума на списък от списъци
;
; Без рекурсивни извиквания!
; HINT: append е вградена
;
; Пример: (sum-lists '((1 2) () (3 4) (5 -15))) ~> 0
(define (sum-lists lsts)
  (apply + (apply append lsts)))

; 2. Декартово произведение
;
; Без рекурсивни извиквания!
(define (cartesian-prod lst1 lst2)
  (apply append
         (map (lambda (x)
                (map (lambda (y) (cons x y))
                     lst2))
              lst1)))

; 3. Средно аритметично на много аргументи
;
; Пример: (avg 1 2 3 4 5) ~> 3
(define (avg h . t)
  (/ (apply + (cons h t))
     (length (cons h t))))

; 4. Дума с максимална дъжлина
;
; HINT: string-length за дължина на низ
; HINT: с помощна функция на много аргументи
;
; Пример: (longest-word '("apple" "banana" "pear")) ~> "banana"
; Пример: (longest-word '("Racket" "is" "powerful")) ~> "powerful"
(define (max-word w . ws)
  (cond
    ((null? ws) w)
    ((< (string-length w)
        (string-length (car ws)))
     (apply max-word (car ws) (cdr ws)))
    (else (apply max-word w (cdr ws)))))

(define (longest-word words)
  (apply max-word words))

; 5. Композиция на на много едноаргументни функции
;
; Пример: ((compose-all (^2) succ) 5) -> 36
(define (compose f g)
  (lambda (x) (f (g x))))

(define (compose-all f . gs)
  (if (null? gs)
      f
      (compose f (apply compose-all gs))))

; 6. Конюнкция на много едноместни предикати
;
; Пример: ((conjoint-all even? >10) 4) -> #t
(define (conjoint p1? p2?)
  (lambda (x) (and (p1? x) (p1? x))))

(define (conjoint-all p? . preds)
  (if (null? preds)
      p?
      (conjoint p? (apply conjoint-all preds))))

; 7. Като map на много аргументи, но трябва да работи за
; списъци с различни дължини.
(define (any? p? lst)
  (foldr* (lambda (x acc) (or (p? x) acc)) #f lst))

(define (zipWith f . lsts)
  (if (or (null? lsts)
          (any? null? lsts))
      '()
      (cons (apply f (map car lsts))
            (apply zipWith f (map cdr lsts)))))
