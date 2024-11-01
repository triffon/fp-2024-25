#lang racket
; От миналия път
(define (sorted? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (< (car lst) (cadr lst))
           (sorted? (cdr lst)))))

(define (all? p? lst)
  (or (null? lst)
      (and (p? (car lst))
           (all? p? (cdr lst)))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (pairs lst)
  (zip lst (cdr lst)))

(define (sorted?* lst)
  (all? (lambda (p) (< (car p) (cdr p)))
        (pairs lst)))

; Примери за използване на foldr
(define (length* lst)
  (foldr (lambda (el res) (+ res 1))
         0
         lst))

; Зад.1
; Тривиално, рекурсивно решение
; Важно - локалната променлива преотвратява двукратно извикване за опашката,
; което би довело до експоненциална сложност.
(define (uniques lst)
  (if (null? lst) '()
      (let [(res (uniques (cdr lst)))]
        (if (member (car lst) res)
            res
            (cons (car lst) res)))))

(define (uniques* lst)
  (foldr (lambda (el res)
           (if (member el res)
               res
               (cons el res)))
         '()
         lst))

; Aлтернативно - филтрираме опашката, преди да се извикваме за нея
(define (uniques** lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (uniques** (filter (lambda (x) (not (equal? x (car lst))))
                               (cdr lst))))))

; Тривиалното итеративно решение, в което само на някои итерации "променяме" резултата
(define (uniques*** lst)
  (define (loop lst res)
    (cond [(null? lst) res]
          [(member (car lst) res) (loop (cdr lst) res)]
          [else (loop (cdr lst) (cons (car lst) res))]))
  (loop lst '()))

; Зад.2
; Модификация с приемане на функция за сравнение по избор
(define (insert-by cmp val lst)
  (cond [(empty? lst) (list val)]
        [(cmp val (car lst)) (cons val lst)]
        [else (cons (car lst)
                    (insert-by cmp val (cdr lst)))]))

; Зад.3, същата модификация
(define (insertion-sort cmp lst)
  (foldr (lambda (el res) (insert-by cmp el res)) '() lst))

; Зад.4
; Помощна функция, подобна на двете по-горе: максимум по даден критерий
(define (maximum-by f lst)
  (if (null? (cdr lst)) ; Празен списък не е валиден аргумент така и така
      (car lst)
      (let [(best (maximum-by f (cdr lst)))]
        (if (> (f (car lst)) (f best))
            (car lst)
            best))))

(define (maximum-by* f lst)
  (foldr (lambda (el res)
           (if (> (f el) (f res)) el res))
         (car lst) ; взимаме първия елемент за "временен" максимум
         (cdr lst)))

; "Методи" за начало, край и дължина на интервал
(define (int-start i)
  (car i))
(define (int-end i)
  (cdr i))
(define (int-length i)
  (- (int-end i) (int-start i)))

; връща дали i1 е подинтервал на i2
(define (int-sub? i1 i2)
  (and (>= (int-start i1) (int-start i2))
       (<= (int-end i1) (int-end i2))))

(define (longest-interval-subsets il)
  (let* [(best (maximum-by* int-length il))
         (subs (filter (lambda (i) (int-sub? i best)) il))]
    (insertion-sort
     (lambda (i1 i2) (< (int-start i1) (int-start i2)))
     subs)))

; Зад.5
(define (group-by f lst)
  ; събиране на всички стойности от lst, за които f връща val
  (define (matches val)
    (filter (lambda (x) (equal? (f x) val)) lst))
  (map (lambda (val) (list val (matches val)))
       (uniques (map f lst))))

(group-by even? '(1 2 3 4 5))

; Зад.6
; Стандартни помощни функции
(define (id x) x)
(define (compose f g)
  (lambda (x) (f (g x))))

(define (sq x) (* x x))
(define (1+ x) (+ x 1))
(define (compose* . fns)
  (foldr compose id fns))
((compose* sq 1+ (lambda (x) (* x 2)) 1+) 5)

; Зад.7
; От миналия път
(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (car lst))
           (any? p? (cdr lst)))))
(define (zipWith* f . lsts)
  (if (or (null? lsts) (any? null? lsts))
      '()
      (cons (apply f (map car lsts))
            (apply zipWith* f (map cdr lsts)))))

(zipWith* max '(1 6 7) '(2 4 9) '(3 5 8 10))
