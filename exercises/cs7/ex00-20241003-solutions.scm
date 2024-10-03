; Зад.1
; Забеелжка: няма нужда да пишем (if ... #t #f)
(define (f1 x y)
  (if (>= y 0)
      (<= (+ (* x x) (* y y)) 4)
      (and (>= y -2) (>= x -1) (<= x 1))))

; Проверява дали (x;y) лежи в квадрат със страна 1
; и долен ляв ъгъл (sqX;sqY)
(define (is-in-square? x y sqX sqY)
  (and (>= x sqX) (<= x (+ sqX 1))
       (>= y sqY) (<= y (+ sqY 1))))

; По употребата на is-in-square? си личи, че
; може би няма нужда от последния си аргумент
(define (f2 x y)
  (or (is-in-square? x y -1 -1)
      (is-in-square? x y 0 0)
      (is-in-square? x y 1 1)))

; Зад.2
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; Зад.3
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; Зад.4
(define (sum-interval a b)
  (if (> a b) ; Удобно дъно e "невалидния" интервал
      0
      (+ a (sum-interval (+ a 1) b))))

; Зад.5
; Понякога най-удобното дъно са едноцифрените числа,
; понякога е числото 0 - а понякога няма значение.
(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

; Зад.6
; Проблем: всеки път преизчисляваме броя на цифрите на n,
; когато на всяко следващо извикване той е със сигурност с 1 по-малко
; от броя по време на предишното извикване
(define (reverse-digits n)
  (if (< n 10)
      n
      (+ (reverse-digits (quotient n 10))
         (* (remainder n 10)
            (expt 10 (- (count-digits n) 1))))))
; Решение: "пазим" го като допълнителен аргумент на помощна функция
; Това е "инвариант" на тази функция: count винаги е точно броят цифри на n
; Забележете, че логиката не се е променила.
(define (reverse-digits-helper n count)
  (if (< n 10)
      n
      (+ (reverse-digits-helper (quotient n 10) (- count 1))
         (* (remainder n 10)
            (expt 10 (- count 1))))))

; Новата "основна" функция само смята броя цифри веднъж
; и оставя всичката логика на помощната функция
(define (reverse-digits* n)
  (reverse-digits-helper n (count-digits n)))

; Зад.7 е най-трудната
(define (palindrome? n)
  (= n (reverse-digits* n)))
