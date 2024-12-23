#lang racket

; В scheme има функции, които могат да приемат
; произволен брой аргументи:
;---------------------------
; Аритметични операции
(+ 1 2 3 4) ; ~> 10
(* 1 2 3 4) ; ~> 24
(- 10 4 5) ; ~> 1
(/ 24 2 3 4) ; ~> 1

; Конструиране на списък
(list 1 2 3 4) ; ~> '(1 2 3 4)

; Q: Какво ако НЕ подадем аргументи?

; Минимална и Максимална стойност
(max 10 15 20 -40 2 3) ; ~> 20
(min 10 15 20 -40 2 3) ; ~> -40

; map на много аргументи
(map + '(1 2 3) '(4 5 6)) ; '(5 7 9)
; списъците трябва да имат еднаква дължина

; Има директен начин да използваме такива операции над списъци.
;
; apply прилага функция над списък от аргументи
(apply max '(10 15 20 -40 2 3)) ; 20

; apply има вида: (apply proc v1 ... vn lst kw-arg ...)
; Засега не се интересуваме от kw-arg.
; Освен подадения списък, apply може да приема и
; допълнителни аргументи към подадената процедура proc.
; Това са онези v1 ... vn

; Тук 2 е допълнителен аргумент към *
(apply * 2 '(1 2 3)) ; 12
; същото като:
(* 2 1 2 3)

; Тук + е допълнителен аргумент към map:
(apply map + '((1 2 3) (4 5 6))) ; '(5 7 9)
; същото като:
(map + '(1 2 3) '(4 5 6))

; Ето и един пример с транспониране на матрица
(define (transpose m)
  (apply map list m))

; Разписваме transpose:
; (apply map list '((1 2 3) (4 5 6) (7 8 9)))
; <=>
; (map list '(1 2 3) '(4 5 6) '(7 8 9))
; <=>
; (list (list 1 4 7)
;       (list 2 5 8)
;       (list 3 6 9))

; Можем да правим функции на произволен брой аргументи
; (lambda (args . opt-args) <body>)
; Където "args" са задължителните параметри,
; а "opt-args" е списък с допълнителни (optional) аргументи.
(lambda (x . lst) (apply + (cons x lst)))

; Вече знаем че define е синтактична захар
; за свързване на символ с ламбда.
;
; Дефинираме функции на много аргументи с define така:
(define (sum x . l) (foldl + 0 (cons x l)))

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
(define (sum-lists lsts) 'undefined)

; 2. Декартово произведение
;
; Без рекурсивни извиквания!
(define (cartesian-prod lst1 lst2) 'undefined)

; 3. Средно аритметично на много аргументи
;
; Пример: (avg 1 2 3 4 5) ~> 3
(define (avg h . t) 'undefined)

; 4. Дума с максимална дъжлина
;
; HINT: string-length за дължина на низ
; HINT: с помощна функция на много аргументи
;
; Пример: (longest-word '("apple" "banana" "pear")) ~> "banana"
; Пример: (longest-word '("Racket" "is" "powerful")) ~> "powerful"
(define (longest-word words) 'undefined)

; 5. Композиция на на много едноаргументни функции
;
; Пример: ((compose-all (^2) succ) 5) -> 36
(define (compose-all f . gs) 'undefined)

; 6. Конюнкция на много едноместни предикати
;
; Пример: ((conjoint-all even? >10) 4) -> #t
(define (conjoint-all p? . preds) 'undefined)

; 7. Като map на много аргументи, но трябва да работи за
; списъци с различни дължини.
(define (zipWith f . lsts) 'undefined)
