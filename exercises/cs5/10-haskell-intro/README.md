### Опции на компилатора

Ще накараме компилатора да бъде по-стриктен и да ни предупреждава за разни неща, които са предпоставка за грешки:

```
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
```

### Основни неща

#### Основни типове
- `Bool`
- `Char` (`'a'`, `'я'`)
- `Int` - цяло число
- `Integer` - цяло число с неограничен размер (като числата в scheme)
- `Float`, `Double`

#### Типове на функции
- `Int -> Char`
- `Int -> Int -> Int`

#### Типови параметри, параметрични типове
- `foo`, `bar`, `foo -> bar`

#### Съставни типове
- `[foo]` - списък от `foo`-та
    - `[1, 2, 3] :: [Int]`
- `(foo, bar)` - tuple с първи елемент от тип `foo` и втори елемент от тип `bar`
    - `(1, pow) :: (Int, Float -> Float)`
- `String = [Char]`

#### Дефиниране
- константа:
```
    foo :: Int
    foo = 42
```

- функция:
```
    add :: Int -> Int -> Int
    add x y = x + y
```

#### Условие
- `if FOO then BAR else BAZ`
- guards

### Задачи

Имплементирайте следните функции:

1. `fact :: Int -> Int`, където `fact n` изчислява факториел от `n`
1. `fib :: Int -> Int`, където `fib n` изчислява n-тото число на фибоначи
1. `myAbs :: Int -> Int`, където `abs n` изчислява модул от n
1. `composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)`, където `composeInt f g` връща функция, която е композицията на едноаргументните функции върху цели числа `f` и `g`
1. `compose`, която е като `composeInt`, но работи за произволни входни данни (измислете типа)
1. `myConcat :: [a] -> [a] -> [a]`, която съединява два списъка
1. `isIntPrefix :: [Int] -> [Int] -> Bool`, която проверява дали първият подаден списък е префикс на втория
1. `isPrefix :: (Eq a) => [a] -> [a] -> Bool`, която работи като `isIntPrefix`, но за произволни входни данни (тук трябва обяснение за типови класове)
1. `frepeat :: Int -> (a -> a) -> a -> a`, където `repeat n f x` е резултата от прилагането на `f` върху `x` `n` пъти
1. `frepeated`, където `repeat n f` връща функция, която действа като `n` пъти прилагане на `f` (измислете типа)

