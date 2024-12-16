Всяка задача носи по **4т**. Решенията носят бонус **1т** ако са оценени с поне **2/4т** и са написани на **Haskell**

---

1. Да се реализира функция `prune`, която приема и връща дърво от положителни
   цели числа с произволен брой разклонения.

```haskell
data Tree = Empty | Node Int [Tree]
    deriving (Show)

prune :: Tree -> Tree
prune = undefined
```

Резултата от функцията трябва да бъде такова дърво, че наследниците на всеки
връх са непразни и взаимно прости с родителя си.

```haskell
-- Пример:
tree :: Tree
tree =
  Node
    10
    [ Node 3 [Empty, Node 3 [], Node 2 [Empty, Empty]],
      Node 7 [Node 14 [], Node 2 [], Node 1 [Empty], Node 15 [Empty]],
      Node 20 [Node 9 [Node 10 []]]
    ]

result :: Tree
result =
  Node
    10
    [ Node 3 [Node 2 []],
      Node 7 [Node 2 [], Node 1 [], Node 15 []]
    ]

prune tree = result
```

2. Да се реализира функция `mergeFromMaybes`, която приема два безкрайни списъка с
   елементи от тип `Maybe a`, слива ги и връща нов безкраен списък с елементи от
   тип `a`.

```haskell
mergeFromMaybes :: [Maybe a] -> [Maybe a] -> [a]
mergeFromMaybes = undefined
```

Сливането се случва като за всеки два елемента **x<sub>i</sub>** и
**y<sub>i</sub>** от входните списъци **x<sub>1</sub>,x<sub>2</sub>,...** и
**y<sub>1</sub>,y<sub>2</sub>,...** в резултата се включи първата стойност от
двата елемента (`Nothing` е липса на стойност). Тоест за `x` = **x<sub>i</sub>**
и `y` = **y<sub>i</sub>** :

- Ако `x = Just u` и `y = Just v`, то `u` се включва в резултата
- Ако `x = Just u` и `y = Nothing`, то `u` се включва в резултата
- Ако `x = Nothing` и `y = Just v`, то `v` се включва в резултата
- Ако `x = Nothing` и `y = Nothing`, то нищо не се добавя в резултата

```haskell
-- Пример:
mergeFromMaybes [Just 1, Nothing, Just 3,  Nothing, Just 5,  Nothing, ...]
                [Just 2, Nothing, Nothing, Just 8,  Nothing, Nothing, ...]
              = [1,               3,       8,       5, ...]
```
