# Въведение в Haskell

### Задача 1
Дефинирайте функции, които връщат:
 - по-малкото от две цели числа
    - добавете тест с отрицателни числа
    - използвайки `if-else`
    - използвайки guards
    - използвайки вградена функция
 - последната цифра на цяло число
 - частното и остатъка при деледението на две цели числа
 - цяло число без последната му цифра
 - частното при деледението на две реални числа
 - средното аритметично на две цели числа
 - числото, закръглено до два знака след десетичната запетая

```haskell
print $ minIf 15 60 == 15
print $ minIf 60 15 == 15
print $ minGuard 15 60 == 15
print $ minGuard 60 15 == 15
print $ minBuiltIn 60 15 == 15

print $ lastDigit 154 == 4

print $ quotientWhole 64 2 == 32
print $ divWhole 154 17 == 9.058823529411764

print $ removeLastDigit 154 == 15    

print $ divReal 154.451 10.01 == 15.42967032967033
print $ quotientReal 154.21 17.17 == 8

print $ avgWhole 5 1542 == 773.5

print $ roundTwoDig 3.1413465345321 == 3.14
print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14
```

### Задача 2
Дефинирайте функция, изчисляваща `n!`:
 - чрез линейно рекурсивен процес
 - чрез линейно итеративен процес
 - чрез списък

```haskell
print $ factRec 11 == 39916800
print $ factIter 11 == 39916800
print $ factXs 11 == 39916800
```

### Задача 3
Дефинирайте функция, изчисяваща `n`-тото число на Фибоначи (0, 1, 1, 2, 3, 5, 8, 13, 21, ...):
 - чрез дървовидно рекурсивен процес
 - чрез линейно итеративен процес

```haskell
print $ fibRec 11 == 89

print $ fibIter 11 == 89
print $ fibIter 110 == 43566776258854844738105
```

### Задача 4
Дефинирайте предикат, който проверява дали число `x` е в затворения интервал `[start;end]`:
 - с булев израз
 - със списък
 - с ламбда

```haskell
print $ isInsideNoLists 5 1 4 == True
print $ isInsideNoLists 10 50 20 == True
print $ isInsideNoLists 10 50 1 == False

print $ isInsideLists 5 1 4 == True
print $ isInsideLists 10 50 20 == True
print $ isInsideLists 10 50 1 == False

print $ (isInsideLambda 5 1) 4 == True
print $ (isInsideLambda 10 50) 20 == True
print $ (isInsideLambda 10 50) 1 == False
```

### Задача 5
Дефинирайте функция, която изчислява средното аритметично на квадратите на две цели числа.

```haskell
print $ sqAvg 5 0 == 12.5
print $ sqAvg 10 13 == 134.5
```

### Задача 6
Дефинирайте предикат, който проверява дали едно неотрицателно число е палиндом.

```haskell
print $ isPalindrome 6 == True
print $ isPalindrome 1010 == False
print $ isPalindrome 505 == True
print $ isPalindrome 123321 == True
print $ isPalindrome 654 == False
```

### Задача 7
Две числа са приятелки, ако сумата на делителите на едното от тях е равна на другото. Дефинирайте предикат, който проверява дали две числя са приятелски.

> Забележка: Използвайте **list comprehension**

```haskell
print $ areAmicable 200 300 == False
print $ areAmicable 220 284 == True
print $ areAmicable 284 220 == True
print $ areAmicable 1184 1210 == True
print $ areAmicable 2620 2924 == True
print $ areAmicable 6232 6368 == True
```

### Задача 8
Дефинирайте предикат, който проверява дали дадено число е просто.

> Забележка: Използвайте **list comprehension**

```haskell
print $ isPrime 1 == False
print $ isPrime 2 == True
print $ isPrime 3 == True
print $ isPrime 6 == False
print $ isPrime 61 == True
```

### Задача 9
Едно число е перфектно, ако е естествено и сумата на неговите делители без самото число е равна на самото число. Дефинирайте предикат, който проверява дали едно число е перфектно.

> Забележка: Използвайте **list comprehension**

```haskell
print $ isPerfect 1 == False
print $ isPerfect 6 == True
print $ isPerfect 495 == False
print $ isPerfect 33550336 == True
```
