# Упражнение 1

# Типове данни

## Булеви стойности - `#t`, `#f`

```scheme
(not #f) ;; => #t
(not #t) ;; => #f

;; Scheme третира всички стойности, които не са #f, като #t
(not "Hello, World!") ;; => #f
(not 0)               ;; => #f
(not '())             ;; => #f
```

```scheme
;; връща последната стойност, ако всички са истина
;; иначе връща #f
(and #t #t) ;; => #t
(and 1 2)   ;; => 2
(and #f 1)  ;; => #f

;; изразите се оценяват отляво-надясно
;; в момента, в който резултатът от and е ясен, изчислението спира
(and 1 #f expression-guaranteed-to-cause-error) ;; => #f
```

```scheme
;; връща първата стойност, която е истина (не е `#f`)
;; иначе връща #f
(or #f #f) ;; => #f
(or 1 2)   ;; => 1
(or #f 1)  ;; => 1

;; изразите се оценяват отляво-надясно
;; в момента, в който резултатът от or е ясен, изчислението спира
(or 1 #f expression-guaranteed-to-cause-error) ;; => 1
```

## Числа - `integers (42)`, `rationals (22/7)`, `reals (3.1416)`, or `complex (2+3i)`

```scheme
;; сравнение
(= 42 42.0)    ;; => #t
(= 42 43)      ;; => #f
(< 3 2)        ;; => #f
(>= 4.5 3)     ;; => #t
(< -5 -1 5 10) ;; => #t

(max 1 3 4 2 3) ;; => 4
(min 1 3 4 2 3) ;; => 1
```

```scheme
;; аритметични операции

;; работят с произволен брой аргументи
(+ 1 2 3) ;; => 6
(- 5.3 2) ;; => 3.3
(* 1 2 3) ;; => 6
(/ 6 3)   ;; => 2
(/ 22 7)  ;; => 22/7

(expt 2 3)   ;; => 8
(expt 4 1/2) ;; => 2.0

(abs  3) ;; => 3
(abs -4) ;; => 4

(quotient 5 2)  ;; => 2
(remainder 5 2) ;; => 1

(round 2.718)   ;; => 3
(ceiling 2.718) ;; => 3
(floor 2.718)   ;; => 2
```

## Знаци

```scheme
;; Започват с `#\`
#\c       ;; Символът `c`
#\newline ;; Нов ред
#\tab     ;; Таб
#\space   ;; Спейс (много иновативно, знам)
```

## Низове

```scheme
"Hello, World!" ;; => "Hello, World!"

(string #\h #\e #\l #\l #\o)          ;; => "hello"
(string-append "Cats " "are " "cute") ;; => "Cats are cute"
(make-string 3 #\a)                   ;; => "aaa"

(substring "string" 1 3) ;; => "tr"
```

# Дефиниране на *глобални* *променливи*

```scheme
;; чрез `define` свързваме символ със стойност
;;(define 'symbol expression)
(define greeting "Hello!")
```

# Дефиниране на функции

```scheme
(define (function-name parameters)
  body)

(define (add1 x)
  (+ x 1))

(define (square x)
  (* x x))

;; Това всъщност е захар за следното …
(define square
  (lambda (x)
    (* x x)))
;; … но за това какво е `lambda` ще си говорим по-нататък :D
```

# Условни изрази

```scheme
;; За разлика от повечето програми езици, с които се се сблъсквали преди (C, C++, Java, JS, Python, ...)
;; в Scheme конструкцията `if` по-скоро играе ролята на тернарен оператор:
;; демек, при "истинност" на `test-expression` цялата форма се оценява до `then-expression`
;; а иначе до `else-expression`.
;; Отделно от това може в тези expression-и да натепаме някакви странични ефекти като принтене на екрата,
;; но за това по-нататък (за любопитните, вижте специалната форма `do`)
(if test-expression
    then-expression
    else-expression)

(define (compare-to-1 x)
  (if (> x 1)
    "your number is bigger than 1"
    "your number is equal to or smaller than 1"))
```

```scheme
;; удобно е да използваме cond за вложени if изрази

;; пример за вложен if израз, който може да
;; пренапишем по-чисто, използвайки cond
;; (define (compare-to-1 x)
;;   (if (> x 1) "your number is bigger than 1"
;;     (if (= x 1) "your number is equal to 1"
;;         "your number is smaller than 1")))

;; else хваща всички неописани случаи
;; else може да бъде пропуснат
(define (compare-to-1 x)
  (cond
    ((> x 1) "your number is bigger than 1")
    ((= x 1) "your number is equal to 1")
    (else "your number is smaller than 1")))
```

# Рекурсия

```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

# Коментари

```scheme
;; функция, която събира две числа
(define (add a b)
  (+ a b)) ; използваме + за събиране

#|
(define (subtract a b)
  (- a b))
|#
```

---

# Задачи

1. Дефинирайте фунцкия `(factorial n)`, която пресмята $n!$

2. Дефинирайте фукнция `(fibonacci n)`, която пресмята $n$-тото число на Фибоначи

3. Дефинирайте функция `(sum-interval a b)`, която смята сумата на числата в челочисления интервал $[a,b]$

```scheme
(sum-interval 1 3) ;; => 6
```

4. Дефинирайте функция `(count-digits number)`, която намира броя на цифрита на дадено естествено число

5. Дефинирайте функция `(reverse-digits number)`, която обръща последователността на цифрите на дадено естествено число

```scheme
(reverse-digits 123) ;; => 321
```

6. Дефинирайте функция `(sum-list lst)`, която пресмята сбора на елементите на списъка $lst$.
