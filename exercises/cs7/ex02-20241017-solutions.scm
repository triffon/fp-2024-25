; Зад.0
(define (const c)
  (lambda (x) c))

(define (flip f)
  (lambda (x y) (f y x)))

; Това е функция от по-висок ред, но не връща функция
; т.е. резултатът не е преизползваем :(
(define (fake-compose f g x)
  (f (g x)))

(define (compose f g)
  (lambda (x) (f (g x))))

; Малки, полезни функции
(define (id x) x)
(define (++ x) (+ x 1))

; Зад.1
(define (repeat n f)
  (if (= n 0)
      ; 0 пъти прилагане на f <=> не я прилагаме и не променяме аргумента
      id
      ; не е по-различно от (+ x (repeat (- n 1) x)) за числа, напр.
      (compose f (repeat (- n 1) f))))

; Зад.2
; Бонус - работи и за нечетни k
(define (twist k f g)
  (if (zero? k)
      id
      (compose f (twist (- k 1) g f))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

; Hello, world на рекурсивните функции, все пак
(define (factorial n)
  (accumulate * 1 1 n id ++))

; Зад.3
(define (!! n)
  (accumulate * 1
              (if (odd? n) 1 2) n
              id
              ++))

; Зад.4
(define (nchk n k)
  (/ (factorial n) (factorial k) (factorial (- n k))))

(define (nchk* n k)
  (accumulate * 1
              0 (- k 1)
              (lambda (i) (/ (- n i) (- k i)))
              ++))

; Зад.5
(define (2^ n)
  (accumulate * 1 1 n (const 2) ++))
