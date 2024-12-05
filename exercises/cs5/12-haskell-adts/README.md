#### алгебрични типове

Алгебричните типове без параметър имат семантика на `Enum` в другите езици:

```
data MyBool
    = MyTrue
    | MyFalse
    deriving (Show)
```

```
data Colour
    = Red
    | Green
    | Cyan
    | Magenta
    | Yellow
    | Blue
    deriving (Show)
```

*Забележка*: `deriving (Show)` го пишем за да накараме компилатора да дефинира
функцията `show` за типовете ни, за да можем да ги използваме
по-лесно. Когато говорим за типови класове, ще разберете как точно работи това.

В тези примери, `MyBool` и `Animal` са *алгебрични типове*, а `MyTrue, MyFalse` и
`Cat, Dog, Moose, Elephant` са техни *конструктори*.

Конструкторите могат да имат и параметри:

```
data Shape
    = Circle Float
    | Rectangle Float Float
    | Ngon Int Float
    deriving (Show)
```

Следните изрази са валидни стойности от тип `Shape`:

- `Circle 42.0`
- `Ngon 5 6.0`

Но забележете, че `Circle` е от тип `Float -> Shape`: всеки конструктор реално
е функция, която като аргументи взима стойности от типовете, които са параметри
на конструктора, и "конструира" стойност от алгебричния тип.

За да използваме тези стойности във функции, можем да ги pattern match-ваме:

```
area :: Shape -> Float
area (Rectangle a b) = a * b
...
```

#### Задачи

**Зад\. 1**: Довършете функцията `area` от по-горе

**Зад\. 2**: Напишете функция `perimeter`, аналогична на `area`

**Зад\. 3**: Нека дефинираме следния тип:

```
data RPS
    = Rock
    | Paper
    | Scissors
    deriving (Show)
```
Напишете функция `beats :: RPS -> RPS -> Bool`, която проверява дали първият
аргумент побеждава втория

### Параметрични алгебрични типове

Алгебричните типове могат да имат типови параметри:

```
data Shape n -- така не се ограничаваме до специфичен числов тип
    = Circle n
    | Rectangle n n
    | Ngon Int n
    deriving (Show)

foo :: Shape Float
foo = Circle 42.0
```

И можем да правим рекурсивни алгебрични типове:

```
data List a
    = Cons a (List a)
    | Empty
    deriving (Show)
```

#### Задачи

**Зад\. 4**: Дефинирайте функцията `lmap` за типа `List a`

**Зад\. 5**: Дефинирайте функцията `lfilter` за типа `List a`

**Зад\. 6**: Дефинирайте функцията `lfoldr` за типа `List a`

### Типови класове (много накратко)

Да си дефинираме типов клас `Countable`, който ни позволява да вземем броя на
елементите на даден обект, без да имаме конкретния му тип:

```
class Countable a where
    count :: a -> Int

instance Countable (List a) where
    count (Cons _ l) = (count l) + 1
    count Empty      = 0
```

**Зад\. 7**: Дефинирайте инстанцията `Averageable` за `List Float`:
```
class Averageable a where
    avg :: a -> Float

instance Averageable (List Float) where
    ...
```

В стандартната библиотека имаме дефиниран класа
[show](https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC.Show.html#Show)
горе-долу по следния начин:
```
class Show a where
    show :: a -> String
```
ще го ползваме наготово. Досега видяхме как може да ползваме `deriving (Show)`
за да накараме компилатора да измисли default-на имплементация на `Show`.

Сега ще си дефинираме няколко наши такива инстанции:

```
instance Show RPS where
    show Rock = "🪨"
    show Paper = "🧻"
    show Scissors = "✂️"
```

Можем да дефинираме инстанции, които зависят от други инстанции:

```
instance (Show n) => Show (Shape n) where
    show (Circle f) = "circle with radius " ++ (show f)
    show (Rectangle a b) = "rectangle with sides " ++ (show a) ++ " and " ++ (show b)
    show (Ngon n r) = "ngon with " ++ (show n) ++ " sides and radius " ++ (show r)
```
"ако знаем как да покажем `n`, то знаем как да покажем `Shape n`"

**Зад\. 8**: Дефинирайте инстанцията `Show` за `List a`, така че
`Cons 1 (Cons 2 (Cons 3 Empty))` да изглежда като `List(1 2 3)`

**Зад\. 9**: Дефинирайте тип, представящ двоично дърво с произволни числови (`Num`) стойности по върховете.
Напишете функция, която проверява дали едно такова дърво е балансирано.
