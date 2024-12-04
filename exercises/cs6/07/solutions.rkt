#lang racket

; ## Задача 2
; #### Максимален приход на продукт в магазин

; Магазин има регистър с продажби, в който се записват транзакции. Всяка
; транзакция е представена като наредена двойка `(<име на артикул> .
; <цена>)` и всички транзакции са съхранени в списък. Напишете функция
; `(max-revenue-product transactions)`, която намира артикула, генерирал най-висок
; приход.

; **Пример**: `(max-revenue-product transactions) ~> (<product> . <revenue>)`

(define transactions
  '(
    ("apples" . 10)
    ("apples" . 20)

    ("pears" . 10)

    ("apples" . 30)
    ("apples" . 10)

    ("banitsa" . 5)

    ("pears" . 30)
    ("pears" . 50)

    ("banitsa" . 100)
    ))


(define (aggregate-for-product product-name transactions)

  (define filtered-transactions
    (filter (lambda (t)
              (equal? product-name (car t)))
            transactions))

  (if (null? transactions)
      (cons product-name 0)
      (cons product-name
            (apply +
                   (map (lambda (product-transaction) (cdr product-transaction))
                        filtered-transactions)))))

(define (aggregate-sum transactions)
  (map (lambda (product)
         (aggregate-for-product product transactions))
       (map car transactions)))

(define (max-by f nv lst)
  (define (op x nv)
    (if (> (f x) (f nv))
        x
        nv))
  (foldl nv op lst))

(define (max-revenue-product transactions)
  (max-by cdr '("blank" . -1) (aggregate-sum transactions)))

; Вариант с вградената в racket (НЕ и в R5RS) group-by. Писана е на упражнения.
(define (max-revenue-product* transactions)
  (max-by cdr
          '("blank" . -1)
          (map (lambda (same-products)
                 (cons (caar same-products)
                       (apply + (map cdr same-products))))
               (group-by car transactions))))


; ## Задача 1
; #### Максимална сума на ниво в двоично дърво

; Напишете функция `(max-sum-level tree)`, която приема двоично дърво от числа
; и намира нивото с максимална сума на стойностите на възлите.

; **Пример**: `(max-sum-level tree) ~> (<level> . <sum>)`

(define (root t) (car t))
(define (left t) (cadr t))
(define (right t) (caddr t))

(define (make-tree root left right)
  (list root left right))

(define (level n tree)
  (cond ((empty? tree) '())
        ((zero? n) (list (root tree)))
        (else (append (level (- n 1) (left tree))
                      (level (- n 1) (right tree))))))

(define (depth tree)
  (if (null? tree)
      0
      (+ 1 (max (depth (left tree)) (depth (right tree))))))

; NOTE: Има вградена функция range в racket (НЕ и в R5RS)
(define (from-to from to)
  (if (> from to)
      '()
      (cons from (from-to (+ from 1) to))))

(define (max-sum-level tree)
  (max-by cdr
          '("blank" . -1)
          (map (lambda (i)
                 (cons i (apply + (level i tree))))
               (from-to 1 (depth tree)))))
