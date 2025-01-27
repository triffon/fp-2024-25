# Седмица 04 - Списъци

## Задача 01 - Основни функции за работа със списъци
Реализирайте ваши версии на вградените функции за работа със списъци: `length`, `member?`, `append`, `reverse`.

## Задача 02 - Индексиране
Напишете функция `at`, която приема списък и число `n`, и връща елемента на позиция `n` в списъка.

### Пример:
```scheme
(at '(1 2 3) 0) ; -> 1
(at '(1 2 3) 3) ; -> undefined
```

## Задача 03 - Взимане/премахване на част от списък
Напишете функциите `take-n` и `drop-n`, които приемат списък и число `n`, и съответно взимат и премахват първите `n` елемента на списъка.

### Пример:
```scheme
(take-n 3 '(1 2 3 4 5)) ; -> (1 2 3)
(take-n 10 '(1 2 3 4 5)) ; -> (1 2 3 4 5)
(drop-n 3 '(1 2 3 4 5)) ; -> (4 5)
(drop-n 10 '(1 2 3 4 5)) ; -> ()
```

## Задача 04 - Търсене по предикат
Напишете функция `find`, която приема списък и предикат, и намира първия елемент в списъка, който отговаря на предиката.

```scheme
(find '(5 7 13 3 6 23 18 11) even?) ; -> 6
(find '(5 7 13 3 6 23 18 11) (lambda (x) (< x 0))) ; -> #f
```

## Задача 05 - Проверка за сортираност
Напишете функция `sorted?`, която приема списък, и проверява дали списъкът е сортиран.

### Пример:
```scheme
(sorted? '(14 25 68 112)) ; -> #t
(sorted? '(14 25 18 112)) ; -> #f
```

## Задача 06 - Сливане
Напишете функция `zip`, която приема 2 списъка, и връща списък от наредени двойки, където първият елемент на двойката на позиция `i` е елементът в първия списък на позиция `i`, а вторият елемент е съответно елементът на позиция `i` във втория.

### Пример:
```scheme
(zip '(1 2 3 4) '("a" "b" "c")) ; -> '((1 . "a") (2 . "b") (3 . "c"))
```

## Задача 07 - Най-дълъг подсписък
Напишете функция `max-repeated`, която приема списък, и връща дължината на най-дългия подсписък, който съдържа еднакви елементи.

### Пример:
```scheme
(max-repeated '(1 1 2 3 3 3 4 2 2 2 2 1 1)) ; -> 4
```

За решението на всяка от следващите задачи използвайте следните реализации на `foldr` и `foldl`, без да използвате директна рекурсия (където е възможно):
```scheme
(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))
```

## Задача 08 - `foldr1` и `foldl1`
Напишете функциите `foldr1` и `foldl1`, които правят същото като `foldr` и `foldl`, но не приемат начална стойност.

## Задача 09 - `filter` и `map`
Реалзирайте функциите `filter`, `map`, `all?` и `any?` чрез `foldr` и `foldr`.

## Задача 10 - Уникални елементи
Напишете функция, която премахва повтарящите се елементи в списък. Редът на елементите в резултата няма значение.

### Пример:
```scheme
(unique '(1 4 1 2 #t "hello" 1 "scheme" #t "world" "hello")) ; -> '(4 2 1 "scheme" #t "world" "hello")
```

## Задача 11 - Обединение и сечение
Напишете функции `union` и `intersection`, които правят съответно обединение и сечение на два списъка. Редът на елементите в резултата няма значение.

### Пример:
```scheme
(union '(1 2 4 "a" #t) '(5 6 1 "b" #f "a")) ; -> '(1 2 4 "a" #t 5 6 "b" #f)
(intersection '(1 2 4 "a" #t) '(5 6 1 "b" #f "a")) ; -> '(1 "a")
```

## Задача 12 - Reverse
Реализирайте функцията `reverse`, чрез `foldr` и `foldl`.

### Пример:
```scheme
(reverse* '(9 1 "A" 6 #f 25)) ; -> '(25 #f 6 "A" 1 9)
```

## Задача 13 - Сливане с функция
Напишете функция `zipWith*`, която приема функция и два списъка. Функцията да връща нов списък, който е равен на поелементното прилагане на функцията над двата списъка.

### Пример:
```scheme
(zipWith* + '(4 8 5) '(3 9 1 2)) ; -> '(7 17 6)
```

## Задача 14 - Вмъкване
Напишете функция, която вмъква елемент на правилното му място в сортиран списък.

### Пример:
```scheme
(insert 5 '(1 4 10)) ; -> '(1 4 5 10)
(insert 12 '(1 4 10)) ; -> '(1 4 10 12)
```

### Бонус:
Напишете функция, която сортира списък чрез алгоритъма `insertion sort`, която да ползва горната функция.

## Задача 15 - Quick Sort
Напишете функция, която сортира списък чрез алгоритъма `quick sort`.

### Пример:
```scheme
(quick-sort '(4 7 2 6 1)) ; -> '(1 2 4 6 7)
```

## Задача 16 - Най-малък елемент в списък
Напишете функция, която намира най-малкия елемент в списък. Задачата да се реши без каквато и да е рекурсия.

## Задача 17 - Средно аритметично
Напишете функция, която смята средното аритметично на произволен брой аргументи.

### Пример:
```scheme
(average 1 2 3 4 5) ; -> 3
```

## Задача 18 - Вариадичен `map`
Реализирайте вариант на `map`, който работи над произволен брой списъци.

### Пример:
```scheme
(map-var + '(1 2 3) '(4 5 6) '(7 8 9)) ; -> '(12 15 18)
```