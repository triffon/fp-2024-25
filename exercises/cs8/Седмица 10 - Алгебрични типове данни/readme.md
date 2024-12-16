# Седмица 10 - Алгебрични типове данни

## Задача 01 - Потребителски типове
Напишете клас `Number`, който наследява `Eq` и `Show` и съдържа следните функции:
- `plus :: t -> t -> t`
- `mult :: t -> t -> t`
- `fromInt :: Int -> t`

Напишете следните потребителски типове:
- `Nat` - представлява естествено число. За типа направете инстанции на класовете `Ord`, `Show` и `Number` (`fromInt` за отрицателни числа да връща числото по модул);
- `Complex` - представлява комплексно число. За типа направете инстанция на класовете `Number` и `Show`;
- `Matrix` - представлява матрица. За типа направете инстанции на класовете `Functor`, `Foldable` и `Show`;
- `BinaryTree` - представлява двоично дърво. За типа направете инстанции на класовете `Functor`, `Foldable` и `Show`;
- `BST` - представлява двоично наредено дърво. За типа направете инстанции на класовете `Foldable` и `Show`;
- `Map` - представлява структурата от данни `map`. Направете 2 реализации - чрез асоциативен списък и чрез двоично наредено дърво. За типа направете инстанции на класовете `Functor`, `Foldable` и `Show`;
- `Graph` - представлява граф, реализиран чрез спсисък на съседство. За типа направете инстанция на класа `Show`.

## Задача 02 - Типът Maybe
Напишете следните функции, като използвате типа `Maybe`:
- `fromInt' :: Int -> Maybe Nat` - превръща `Int` число в `Nat`, като ако числото е отрицателно да връща `Nothing`;
- `sub :: Nat -> Nat -> Maybe Nat` - намира разликата на 2 естествени числа, само когато тя е естествено число;
- `find :: (t -> Bool) -> [t] -> Maybe t` - намира първото срещане на елемент в списък по подаден предикат;
- `slice :: String -> Nat -> Nat -> Maybe String` - по подаден символен низ и две естествени числа `start` и `end`, връща частта от низа, в която елементите са между индексите [start, start + end). Ако интервала не е дефиниран коректно, функцията да връща `Nothing`.

## Задача 03 - Валидации
Напишете функция, която по подадени данни от попълнена форма за регистрация, прави валидация на попълнените данни. Формата за регистрация има полета за потребителско име, имейл, парола и повтаряне на паролата. Потребителското име трябва да е поне 6 символа, имейлът трябва да е валиден имейл (за простота нека това значи че трябва да съдържа един символ @ и поне един символ . след него), паролата и повторението на паролата трябва да съвпадат и да съдържат поне 6 символа, от които поне една малка буква, една главна буква и една цифра. При успешна валидация, фунцкията да връща нов потребител, съдържащ потребителско име, имейл и парола. В противен случай да връща подходящо съобщение, спрямо вида на първата срещната грешка:

- "Invalid username! Username should be at least 6 symbols!"
- "Invalid email! Email should be a valid email!"
- "Invalid password! Password should be at least 6 symbols and should contain at least one lowercase letter, one uppercase letter and one digit!"
- "Passwords do not match!"

```haskell
testTree::BinaryTree Integer
testTree = Node 5 
                (Node 1 
                      (Node 4 
                            Empty 
                            (Node 13 Empty Empty)) 
                      (Node 3 Empty Empty)) 
                (Node 8 
                      (Node 0 
                            (Node 10 Empty Empty) 
                            (Node 9 Empty Empty)) 
                      (Node 11 Empty Empty))

bst :: BST Integer
bst = BSTNode 3 (BSTNode 1 
                         BSTEmpty 
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 4 
                         BSTEmpty 
                         (BSTNode 5 BSTEmpty BSTEmpty))
```

## Задача 04 - Ротации
Напишете функции, които правят лява и дясна ротация на двоично дърво.

### Пример:
```haskell
ghci> rotateLeft testTree -- Node 8 (Node 5 (Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 3 Empty Empty)) (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty))) (Node 11 Empty Empty)
ghci> rotateRight testTree -- Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 5 (Node 3 Empty Empty) (Node 8 (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 11 Empty Empty)))
```

## Задача 05 - Нови листа
Напишете функция, която заменя всяко листо на двоично дърво с дърво, съдържащо само 2 листа, като всички възли в него имат стойност, същата като тази на замененото листо.

### Пример:
```haskell
ghci> bloom testTree -- Node 5 (Node 1 (Node 4 Empty (Node 13 (Node 13 Empty Empty) (Node 13 Empty Empty))) (Node 3 (Node 3 Empty Empty) (Node 3 Empty Empty))) (Node 8 (Node 0 (Node 10 (Node 10 Empty Empty) (Node 10 Empty Empty)) (Node 9 (Node 9 Empty Empty) (Node 9 Empty Empty))) (Node 11 (Node 11 Empty Empty) (Node 11 Empty Empty)))
```

## Задача 06 - Път от корен до листо
Напишете функция, която по подадено двоично дърво, връща списък от всички пътища от корена до някое листо на дървото.

### Пример:
```haskell
ghci> paths testTree -- [[5,1,4],[5,1,4,13],[5,1,3],[5,8,0,10],[5,8,0,9],[5,8,11]]
```

## Задача 07 - Най-близък родител
Напишете функция, която по подадени 2 елемента и двоично дърво намира най-близкия общ родител на двата елемента в дървото. Ако няма такъв, функцията да връща `Nothing`.

### Пример:
```haskell
ghci> lowestCommonAncestor 9 11 testTree -- Just 8
ghci> lowestCommonAncestor 9 3 testTree -- Just 5
ghci> lowestCommonAncestor 9 50 testTree -- Nothing
```