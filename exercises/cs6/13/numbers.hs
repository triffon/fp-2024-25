import Prelude hiding (Int)
-- Естествени числа
data Nat
  = Zero
  | Succ Nat
  deriving (Show)

-- Цели числа представяме като разлика на две естествени числа.
--
-- Whole (5 :: Nat) Zero = 5 - 0 = 5
-- Whole Zero (5 :: Nat) = 0 - 5 = -5
--
-- Имайте предвид че представянето не е единствено 6 - 2 = 5 - 1 = 4 - 0
data Whole = Whole Nat Nat
  deriving (Show)

-- Имплементирайте сравнение на цели числа,
-- Без да конвертирате към други типове!
-- Deps: Eq Nat, Num Nat (+)
instance Eq Whole

-- Рационални числа (дроби) представяме
-- като двойка цели числа - числител и знаменател
--
-- TODO: знаменателя може да е 0
data Quot = Quot Whole Whole
  deriving (Show)

-- Имплементирайте сравнение на рационални числа.
-- Имайте предвид че представянето не е единствено: 1/2 = 2/4 = 4/8 = ...
-- Deps: Num Whole (*) (+), Eq Whole
instance Eq Quot

-- Типовете, които са инстанции на Enum,
-- са такива че можем да изброяваме елементите им.
--
-- TODO: Кога едно безкрайно множество е изброимо?
-- TODO: Какви са методите на Enum?
--
-- Пример:
-- >>> take 3 [(Zero)..]
-- [Zero, Succ Zero, Succ (Succ Zero)]
instance Enum Nat where

-- Deps: Enum Nat, Num Nat fromInteger
instance Enum Whole where

instance Enum Quot where

-- Hint: За fromEnum Може да ви потрябват следните:
natToInt :: Nat -> Int
natToInt = undefined

wholeToInt :: Whole -> Int
wholeToInt = undefined
