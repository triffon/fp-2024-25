# 12. Алгебрични Типове Данни (ADTs) и Класове (typeclasses)

## Типови променливи

Позволяват ни да обработваме елементи от различен типове по универсален начин.

```haskell
-- head е функция, която приема списък от каквито и да е елементи и връща първия елемент
-- "a" е типова променлива и може да бъде от всеки тип
-- >>> :t head
-- head :: [a] -> a

-- fst е функция, която приема наредена двойка от каквито и да е елементи и връща първия елемент
-- "a" и "b" са типови променливи и могат да бъдат от всеки тип
-- >>> :t fst
-- fst :: (a, b) -> a
```

## Tипове данни

### Синоним на тип

Типовите синоними ни позволяват да задаваме нови имена на вече съществуващи типове. Използваме ги за по-добра четимост на кода.

```haskell
-- еквивалентни и взаимнозаменяеми
type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- типовите синоними могат да бъдат параметризирани
type AssocList k v = [(k, v)]
```

### Потребителски дефинирани типове

Дефинираме нови типове с `data <Име-на-типа> = <Конструктор(и)>`

```haskell
data Bool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- Circle приема 3 аргумента от тип Float: координати на центъра на кръга и радиус
-- Rectangle приема 4 аргумента от тип Float: координатите на горния ляв и долния десен ъгли на правоъгълника

-- функция, която приема форма и връща лицето ѝ
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

-- >>> surface $ Circle 10 20 10
-- 314.15927

-- >>> surface $ Rectangle 0 0 10 10
-- 100.0
```

### Записи с полета

```haskell
-- тип Car, който се дефинира с компанията, модела и годината си
data Car = Car String String Int
```

**Проблеми:**
- Не е очевидно какво се очаква да получи конструкторът

```haskell
- Трябва сами да си дефинираме "getters" за отделните полета на типа:
company :: Car -> String
company (Car company _ _) = company
```

По-добър вариант - запис с полета:

```haskell
data Car = Car { company :: String, model :: String, year :: Int}

-- автоматично създава gettres company, model и year
-- >>> car = Car { company="Ford", model="Mustang", year=1967 }
-- >>> company car
-- >>> model car
-- >>> year car
-- "Ford"
-- "Mustang"
-- 1967
```

### Типови параметри

Конструкторите на стойности приемат като аргумент стойности и връщат нова стойност. По същия начин, конструкторите на типове приемат като аргумент типове и връщат нов тип.

```haskell
data Maybe a = Just a | Nothing

data Either a b = Left a | Right b
```

Никоя стойност не може да е от тип Maybe или Either, но може да бъде от тип `Maybe Int`, `Maybe String`, `Either Int b`, etc...

```haskell
-- >>> :t Just 'a'
-- Just 'a' :: Maybe Char

-- >>> :t Left (12::Int)
-- Left (12::Int) :: Either Int b

-- >>> :t Right "ZZZ"
-- Right "ZZZ" :: Either a String
```


## [Класове от типове](http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101)

Клас от типове наричаме интерфейс, който дефинира някакво поведение.

```haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)
```

Някои основни класове от типове са:

- `Eq` - включва типовете, чиито стойности могат да се сравняват, използвайки `==` и `/=`
- `Ord` - включва типовете, чиито стойности имат наредба и могат да се сравняват използвайки `>`, `<`, `>=`, `<=` и `compare`
- `Enum` - включва типовете, чиито стойности могат да бъдат изброени - изполваме ги в `range`, имат дефинирани предшественик (`pred`) и наследник (`succ`)
- `Num` - включва числените типове
- `Integral` - включва типовете `Int` и `Integer`
- `Floating` - включва типовечт `Float` и `Double`
- `Show` - включва типовете, чиито стойности могат да бъдат представени като низове, използвайки `show`
- `Read` - включва типовете, чиито стойности могат да бъдат прочетени от низове, използвайки `read`

```haskell
> :info Eq -- изрежда всички класове, които са инстанции на класа Eq (примерно: Int, Char, Bool)
```

### Класови ограничения

**Пример:** Функцията за сравнение `==` приема два аргумента от един и същи _тип_ `a`, който принадлежи на _класа_ от типове `Eq` (стойности, които могат да бъдат сравнявани), и връща булева стойност:

```haskell
-- >>> :t (==)
-- (==) :: Eq a => a -> a -> Bool
```

### Дефиниране на инстанции на клас от типове

Даден тип може да бъде направен инстанция на клас ако поддържа/имплементира неговото поведение. Това ни позволява да използваме обекти от дадения тип във всички функции, които са дефинирани върху инстанции на класа.

```haskell
data TrafficLight = Red | Yellow | Green

-- Правим дефинирания от нас тип Traffic Light инстанция на класа от типове Eq.
-- Така данни от тип TrafficLight ще могат да бъдат сравнявани с (==) и (/=)
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- Правим дефинирания от нас тип Traffic Light инстанция на класа от типове Show
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
```

### Автоматични инстанции на клас от типове

Haskell може автоматично да направи типа ни инстанция на всеки от следните класове от типове:  `Eq`, `Ord`, `Enum`, `Show`, `Read`. За целта използваме `deriving`.

```haskell
data Person = Person {firstName :: String, lastName :: String, age :: Int}
  deriving (Eq, Show, Read)
```


## Рекурсивни типове

1. Списъци

```haskell
data List a = Nil | a `Cons` (List a) deriving (Show, Read)

listLength :: (Integral b) => List a -> b
listLength Nil = 0
listLength (_ `Cons` xs) = 1 + listLength xs

-- >>> listLength (Cons 1 (Cons 2 Nil))
-- 2

listHead :: List a -> Maybe a
listHead Nil = Nothing
listHead (x `Cons` _) = Just x

-- >>> listHead (Cons 1 (Cons 2 Nil))
-- Just 1

instance (Eq a) => Eq (List a) where
  (==) :: Eq a => List a -> List a -> Bool
  Nil == Nil = True
  (x `Cons` xs) == (y `Cons` ys) = (x == y) && (xs == ys)
  _ == _ = False

-- >>> (1 `Cons` (2 `Cons` (3 `Cons` Nil))) == (Cons 1 (Cons 2 (Cons 3 Nil)))
-- True
```

2. Двоични дървета:

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

makeLeaf :: a -> Tree a
makeLeaf x = Node x Empty Empty

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node x l r) =
    Node (f x) (mapTree f l) (mapTree f r)
```

## Задачи

### Записи:

1. Нека са дефинирани типовете:

```haskell
type Student = String  -- име на ученик
type Subject = String  -- име на предмет
type Note = Double     -- оценка

-- Запис с име на ученик, предмет и оценката на ученика по дадения предмет.
data Record = Record {student :: Student, subject :: Subject, note :: Note}
  deriving (Read, Show)
```

Дефинирайте функцията `goodStudentsAverage:: [Record] -> Note`, която връща средния успех на всички ученици, които имат поне една шестица.

2. Нека са дефинирани следните типове:

```haskell
type Name = String      -- име на отбор и играч
type Goals = Int        -- брой отбелязани попадения
type Assists = Int      -- брой асистенции
type Hometown = Name    -- име на града на даден отбор

-- описание да играч
data Player = Player {playerName :: Name, goals :: Goals, assists :: Assists}
  deriving (Read, Show)

-- описание на отбор
data Team = Team {teamName :: Name, hometown :: Hometown, players :: [Player]}
  deriving (Read, Show)
```

Дефинирайте следните функции:
  - `topScorrer`, която приема списък с отбори и връща името на играча с най-много попадения.
  - `topAssists`, която приема списък с отбори и връща името на играча с най-много асистенции.
  - `topTeam`, която приема списък с отбори и връща името на отбора с най-много попадения.
  - `topCity`, която приема списък с отбори и връща името на града с най-много отбори в него.


### Двоични дървета:

3. Дефинирайте функция `depth tree`, която намира дълбочината на подаденото двоично дърво

4. Дефинирайте функция `countLeaves tree`, която намира броя на листата на подаденото двоично дърво

5. Дефинирайте функции `collectPreOrder tree` и `collectInOrder tree`, които връщат списък от елементите на дървото, обходено съотвено `root-left-right` и `left-root-right`

6. Дефинирайте функция `level tree index`, която връща списък от стойностите на възлите, намиращи се на дълбочина `index` от корена

7. Дефинирайте функция `prune tree`, която премахва всички листа в подаденото дърво

8. Дефинирайте функция `invert tree`, която връща огледалния образ на `tree`

9. Дефинирайте функция `path element tree`, която намира пътя до element в подаденото дърво или връща `[]`, ако такъв не съществува

10. Дефинирайте фунцкия `contains tree path`, която проверява дали пътят `path` - списък от стойности, се съдържа в подаденото дърво

11. Двоично наредено дърво е двоично дърво, в което:
    - елементите, <= от корена, се намират в лявото поддърво
    - елементите, > от корена, се намират в дясното поддърво

    Дефинирайте предикат `isBST tree`, който проверява дали подаденото дървото е наредено.

12. Дефинирайте функция `bstInsert tree element`, която вмъква елемента `element` в подаденото двоичното наредено дърво.
Новополученото дърво трябва да бъде двоично наредено.

13. Дефинирайте функция `treeSort lst`, която сортира подадения списък, използвайки двоично наредено дървo

14. Дефинирайте функциата `treePaths tree`, която връща всички пътища от корена на дървото до листата му.

15. Нека е дадено двоично дърво `tree`. Дефинирайте функцията `findMeanNodes tree`, която връща списък с всички върхове на `tree`, чиято стойност е равна на средното аритметично на родителя (ако има такъв) и децата на дадения връх.

16. Да се дефинира функция `findGrandpas tree`, която за дадено двоично дърво от естествени числа `tree` намира списък от всички числа - върхове на `tree`, които са равни на сумата от внуците си.

17. Дефинирайте функцията `heavyNodes tree`, която връща списък със стойностите на всички върхове на дървото `tree`, които са по-големи от сбора на предшествениците си.
