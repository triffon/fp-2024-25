# Algebraic Data Types

### Задача 1
Дефинирайтe ADT `Shape`, който има следните конструктори:
 - `Circle` - с един аргумент, резпрезентиращ радиуса на кръга
 - `Rectangle` - с два аргумента, резпрезентиращи дължината и ширината на правоъгълника
 - `Triangle` - с три аргумента, неговите дължини
 - `Cylinder` - с два аргумента, радиуса на основата и височината

Създайте по една фигура от всеки тип и я отпечатайте на екрана.

### Задача 2
Дефинирайте следните функцции за `Shape`:
 - `perimeter` - намира обиколката на фигурата
 - `area` - намира лицето на фигурата
 - `isRound` - проверява дали фигурата е кръгла
 - `is2D` - проверява дали фигурата има две измерения

```haskell
print $ perimeter (Circle 5) == 31.41592653589793
print $ perimeter (Rectangle 2.5 4.5) == 14
print $ perimeter (Rectangle 5.5 20.6) == 52.2
print $ perimeter (Triangle 5.3 3.9 4.89) == 14.09
print $ perimeter (Cylinder 2.5 10) == 30

print $ area (Circle 5) == 78.53981633974483
print $ area (Rectangle 2.5 4.5) == 11.25
print $ area (Rectangle 5.5 20.6) == 113.30000000000001
print $ area (Triangle 5.3 3.9 4.89) == 9.127927385194024
print $ area (Cylinder 20 30) == 6283.185307179587  

print $ isRound (Circle 5) == True
print $ isRound (Rectangle 2.5 4.5) == False
print $ isRound (Rectangle 5.5 20.6) == False
print $ isRound (Triangle 5.3 3.9 4.89) == False
print $ isRound (Cylinder 20 30) == True

print $ is2D (Circle 5) == True
print $ is2D (Rectangle 2.5 4.5) == True
print $ is2D (Rectangle 5.5 20.6) == True
print $ is2D (Triangle 5.3 3.9 4.89) == True
print $ is2D (Cylinder 20 30) == False

-- Perimeter of a cylinder: 4 * r + 2 * h.
-- Area of a cylinder: 2 * pi * r * h + 2 * pi * r * r.
```

### Задача 3
Напишете функция `getAreas`, която връща списък от лицата на фигури.

```haskell
print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.54, 11.25, 113.3, 9.13, 6283.19]
```

### Задача 4
Напишете функция `maxArea`, която връща фигурата с най-голямо лице.

```haskell
print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0
```

### Задача 5
Дефинирайте ADT `Point`, който може да има 2 или 3 измерения. Създайте по една точка от всеки тип и я принтирайте. Дефинирайте функция от по-висок ред `getPoints`, която приема две унарни функции `f` и `g` и списък от точки с две координати. Функцията да връща тези точки, за които `f (first coordinate) = g (second coordinate)`.

```haskell
print $ getPoints (\x -> x * x) (2+) [Point2D 2 2, Point2D 1 2, Point2D 3 7] == [Point2D 2 2, Point2D 3 7]
```

### Задача 6
Дефинирайте функцция, кооято намира разстоянието между две точки. Подсигурете, че точките имат еднакъв брой измерения.

```haskell
print $ distance (Point2D 2 5) (Point2D 6 9) == 5.66
print $ distance (Point3D 2 5 10) (Point3D 6 9 (-5)) == 16.03
```

### Задача 7
Дефинирйте функцция, която приема точка `p` и списък от точки. Функцията трябва да връща списък от точките, които са най-близо до `p`.

```haskell
print $ closestTo (Point3D 2 5 10) [(Point3D 4 5 6), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == [Point3D 4.0 5.0 6.0]
```

### Задача 8
Дефинирайте функция, която приема списък от точки и връща наредена тройка `d, p1, p2`, репрезентираща най-близкоото разстояние между които и да е две точки и самите точки.

```haskell
 print $ getClosestDistance [(Point3D 4 5 6), (Point3D 2 5 10), (Point3D 5 2 (-10)), (Point3D (-2) 1 45), (Point3D 12 0 2), (Point3D 6 5 4)] == (2.83, Point3D 4.0 5.0 6.0, Point3D 6.0 5.0 4.0)
```