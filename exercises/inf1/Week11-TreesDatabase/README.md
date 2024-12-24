# Дървета. Множество данни

### Задача 1
Дефинирайте рекурсивен алгебричен тип `BTree`, представящ двоично дърво. Едно BTree може да пази произволен тип данни в своите върхове. Създайте следните дървета и ги отпечатайте:

numberBTree:

![Alt text](assets/task1_1.png?raw=true "task1_1.png")

charBTree:

![Alt text](assets/task1_2.png?raw=true "task1_2.png")

Дефинирайте следните функции:
 - `size` - връща броя на върховете
 - `sumTree` - връща сумата на елементите във върховете (трябва да работи само за дървета, пазещи числа във върховете)
 - `traverseDFS` - връща върховете, използвайки DFS
 - `getLevel` - приема цяло число `k` и връща върховете на ниво `k` (коренът е на ниво 0)
 - `traverseBFS` - връща върховете, използвайки DFS
 - `mapTree` - прилага унарна функция върху върховете на дървото
 - `mirrorTree` - връща симетричното дърво

```haskell
print $ numberBTree
print $ charBTree

print $ size numberBTree == 9
print $ size charBTree == 7

print $ sumTree numberBTree == 146
-- print $ sumTree charBTree -- should not work

print $ traverseDFS numberBTree == [96, 1, 12, 0, 5, 2, 4, 5, 21]
print $ traverseDFS charBTree == "haskell"

print $ getLevel numberBTree 2 == [1, 0, 2, 5]
print $ getLevel charBTree 1 == "al"
print $ getLevel charBTree 3 == []

print $ traverseBFS numberBTree == [5,12,4,1,0,2,5,96,21]
print $ traverseBFS charBTree == "kalhsel"

print $ mapTree numberBTree (*2) == Node 10 (Node 24 (Node 2 (Node 192 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 4 Nil Nil) (Node 10 Nil (Node 42 Nil Nil)))
print $ mapTree charBTree (toUpper) == Node 'K' (Node 'A' (Node 'H' Nil Nil) (Node 'S' Nil Nil)) (Node 'L' (Node 'E' Nil Nil) (Node 'L' Nil Nil))

print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))
```

### Задача 2
Един продукт има име, количество (в грамове/милилитри) и цена (в лева). Един магазин има продукти, които ще използваме за "база от данни".

Наличните продукци в магазина са:
 - Bread: 1000g for 1.20;
 - Milk: 2000ml for 4.50;
 - Lamb: 5000g for 10.00;
 - Cheese: 750g for 5.00;
 - Butter: 1000g for 5.50;
 - Water: 500ml for 0.50;
 - Soap: 250g for 4.50.

Създайте и принтирайте "базата данни".

Дефинирайте следните функции:
 - `total` - връща общата цена на всичко в магазина
 - `buy` - симулира резултата от купуването на продукт. Функцията приема име и количество на даден продукт и проверява дали продуктът е наличен в "базата". Ако желаното количество е равно на наличното количество, продуктът се премахва от магазина. В противен случай, количеството се намаля.
 - `getNeeded` - по дадено число `q` връща всички продукти с количество по-малко или равно на `q`
 - `closestToAverage` - връща продуктите с цена най-близка до средната цена на всички продукти
 - `cheaperAlternatives` - приема име и цена на продукт и връща броя на по-евкините алтернативи

```haskell
type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price
```

```haskell
print $ db
print $ getTotal db == 31.2
print $ "Buying 500 Bread:"
print $ buy "Bread" 500 db
print $ "Buying 500 Water:"
print $ buy "Water" 500 $ buy "Bread" 500 db
print $ "Buying 100 Soap:"
print $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db
print $ "Buying 500 Bread:"
print $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db
-- print $ buy "Water" 100 $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db -- Should give an error.
-- print $ buy "Soap" 200 $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db -- Should give an error.
```

```haskell
db1 :: Database
db1 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50 ]

db2 :: Database
db2 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Lamb" 1000 5.50, Product "Water" 500 0.50, Product "Lamb" 250 4.50 ]
```

```haskell
print $ getAverage db1 == 4.457142857142857
    
print $ getNeeded 750 db1 == [Product "Cheese" 750 5.0,Product "Water" 500 0.5,Product "Soap" 250 4.5]

print $ closestToAverage db1 == ["Milk","Soap"]

print $ cheaperAlternatives "Lamb" 5.50 db2 == 1
print $ cheaperAlternatives "Lamb" 10  db2 == 2
```

### Задача 3
Дефинирайте рекурсивен алгебричен тип `NTree`, представящ N-арно дърво. Едно NTree може да пази произволен тип данни в своите върхове.

![Alt text](assets/task3.png?raw=true "task3.png")

```
t = Node 10 [Node 3 [Node 5 [Nil], Node 8 [Node 1 [Nil], Node 2 [Nil]], Node 9 [Nil]], Node 7 [Node 11 [Nil], Node 13 [Nil]], Node 12 [Node 6 [Nil], Node 4 [Nil]]]
```

Дефинирайте следните функции:
 - `size` - връща броя на върховете
 - `getLevel` - приема цяло число `k` и връща върховете на ниво `k` (коренът е на ниво 0) 

```
print $ t
print $ size t == 13
print $ getLevel t 3 == [1, 2]
```

### Задача 4
Един цвят може да бъде червен, зелен или син. Създайте следното дърво:

![Alt text](assets/task4.png?raw=true "task4.png")

Дефинирайте функция, която приема цвят и връща височината на най-високия връх с този цвят.

```haskell
print $ colourBTree
print $ highest Red colourBTree == 4
print $ highest Green colourBTree == 3
print $ highest Blue colourBTree == 4
```

### Задача 5
Използвайки следните типове, дефинирайте функция, която приема списък от записи и връща най-трудния предмет.

```haskell
type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)
```

```haskell
print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"
```

### Задача 6
Нека `(x, y, z)` е вектор, репрезентиращ върховете набинарно дърво по такъв начин, че `x` да е стойността на текущия връх, а `y` и `z` са стойностите на децата. Дефинирайте функция, която връща листата на тези дървета.

```haskell
print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
```

### Задача 7
Дефинирайте функция, която приема списък от различни цели числа и конструира **максимално двоично дърво**. Едно максимално двоично дърво се конструира по следния начин:
 1. Коренът е най-голямото число в списъка
 2. Лявото поддърво е максимално двоично дърво, конструирано от елементите отляво на максималното число
 3. Дянто поддърво е максимално двоично дърво, конструирано от елементите отдясно на максималното число

![Alt text](assets/task7.png?raw=true "task7.png")

```haskell
print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t2
```

### Задача 8
Върховете на следното изображение представят долна и горна граница на интервал (първото число винаги ще бъде по-малко от второто). Дефинирайте функция, която проверява такова бинарно дърво е наредено спрямо релацията `дължина на интервал`.

![Alt text](assets/task8_1.png?raw=true "task8_1.png")

![Alt text](assets/task8_2.png?raw=true "task8_2.png")

```haskell
print $ ordered t1 == True
print $ ordered t2 == False
```

### Задача 9
Дефинирайте функция, която проверява дали дадена дума е налична в двоично дърво с върхове символи.

```haskell
t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil)) 

print $ containsWord t1 "acd" == True
print $ containsWord t1 "cd" == True
print $ containsWord t1 "af" == False
print $ containsWord t1 "ac" == False
print $ containsWord t1 "acdh" == False
print $ containsWord t1 "b" == False
print $ containsWord t1 "e" == True
print $ containsWord t2 "ab" == True
print $ containsWord t2 "ad" == False
print $ containsWord t3 "bdh" == True
print $ containsWord t3 "bdi" == True
print $ containsWord t3 "ac" == False
```

### Задача 10
Дефинирайте функция, която връща всички възможни думи в двоично дърво с върхове символи.

```haskell
t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil))

print $ genWords t1 == ["acf","acd","abe","cf","cd","f","d","be","e"]
print $ genWords t2 == ["acd","ab","cd","d","b"]
print $ genWords t3 == ["abdh","abdi","abe","acf","acg","bdh","bdi","be","dh","di","h","i","e","cf","cg","f","g"]
```