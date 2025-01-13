-- Типови класове:
------------------
-- Последно се запознахме с понятието типов клас
-- и видяхме как се дефинират алгебрични структури от данни.
--
-- Пример за създаване на типов клас
class MyEq a where
  isEqual :: a -> a -> Bool -- "Трябва" да е релация на еквивалентност

-- Вече споменах аналогията на типовите класове като множества.
-- Можем да правим ограничения и при дефиниране на нов типов клас.
-- Тоест може да си мислим че Eq е подмножество на Ord.
class MyEq a => MyOrd a where
  lessThanOrEqual :: a -> a -> Bool

-- TODO: :i(nfo) Истинските типови класове Eq и Ord от Prelude
-- TODO: Още типови класове и Typeclassopedia

-- Инстанция на типов клас наричаме всеки тип,
-- за който са реализирани операциите зададени в класа.
--
-- Тези операции наричаме методи на съответния клас.
-- Ще видим как можем да направим някой тип инстанция на някой клас.

-- Естествени числа:
data Nat
  = Z
  | Succ Nat
  deriving (Show)

-- Като за начало, това понякога е възможно да се случи и автоматично.
--
-- deriving казва на Haskell да се опита да генерира нужните функции,
-- за да бъде типа инстанция на изброените класове.
-- Като не е задължително това да се случи в дефиницията на типа.
-- (може да сме получили типа от външен модул)
deriving instance Read Nat

-- >>> read "Succ (Succ Z)" :: Nat
-- Succ (Succ Z)

-- Нека видим как да направим Nat инстанция на Eq и Ord.

-- Num:
-------
-- За да кастваме числа към Nat трябва да имплементираме fromInteger.
-- Тоест инстанцията няма да е пълна,
-- но ще се възползваме от удобството да можем да кастваме.
--
-- Пример:
-- >>> 5 :: Nat
-- Succ (Succ (Succ (Succ (Succ Z))))
instance Num Nat where
  -- Обикновено не можем да пишем декларации тук!
  -- Горе съм добавил флаг "InstanceSigs" за да показвам типовете на методите.
  fromInteger :: Integer -> Nat
  fromInteger 0 = Z
  fromInteger n
    | n < 0 = error $ "Expected positive Integer and got: " ++ show n
    | otherwise = Succ $ fromInteger (n - 1)

-- Eq:
------
-- Достатъчно е да се имплементира поне едно от: (==), (/=)
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Z == Z = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False -- Забележете че хванахме дуалните случаи с една клауза

  (/=) :: Nat -> Nat -> Bool
  Z /= Z = False
  (Succ n) /= (Succ m) = n /= m
  _ /= _ = True

-- Свойства за Eq (релация на еквивалентност):
-- За всяко x y z
-- x == x
-- x == y -> y == x
-- x == y && y == z -> x == z

-- Ord:
-------
-- Достатъчно е да се имплементира едно от: compare, (<=)
--
-- compare връща Ordering, а то има следната дефиниция:
-- data Ordering = LT | EQ | GT
--
instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare Z Z = EQ
  compare Z _ = LT
  compare _ Z = GT
  compare (Succ n) (Succ m) = compare n m

  (<=) :: Nat -> Nat -> Bool
  Z <= _ = True
  _ <= Z = False
  (Succ n) <= (Succ m) = n <= m

-- Закони за Ord (частична наредба):
-- За всяко x y z
-- x <= x
-- x <= y && y <= x -> x == y
-- x <= y && y <= z -> x <= z

-- Bitvectors:
--------------
-- Ще си дефинираме битове.
-- На практика Bit е същото като Bool, но ще го използваме в друг контекст.
data Bit = Zero | One
  deriving (Show)

-- Преди си дефинирахме естествени числа с Nat.
-- Тоест бяхме закодирали числата в унарна бройна система.
-- Сега с Bit имаме двоичен запис.

-- Двоично число ще е списък от битове:
data BitVector
  = End
  | BitVector :. Bit
  --          ^ вече сме виждали при списъците, че можем да имаме инфиксни
  --            оператори за конструктори
  --
  -- Тук обаче правим нещо различно - залепяме битовете отдясно.
  -- Защото обикновено така бихме записали двоично число на хартия.
  deriving (Show)

-- Този ред казва че оператора (:.) е ляво асоциативен
-- и декларира някакъв приоритет.
infixl 6 :.

-- Примери:
-----------
-- Number: 0
-- Binary: 0
-- BitVector: End
--
-- Забележете че представянето не е единствено и следните са равни:
-- End
-- End :. Zero
-- End :. Zero :. Zero :. Zero

-- Number: 6
-- Binary: 110
-- BitVector: End :. One :. One :. Zero
--
-- Number: 5
-- Binary: 101
-- BitVector: End :. One :. Zero :. One

-- Изместване с бит надясно:
-- 1101 -> 110
--
-- Пример:
-- >>> shiftRight (End :. One :. One :. Zero :. One)
-- ((End :. One) :. One) :. Zero
shiftRight :: BitVector -> BitVector
shiftRight End = End
shiftRight (bv :. _) = bv

-- Не забравяйте че (:.) е ляво асоциативна:
-- >>> End :. One :. Zero :. One
-- ((End :. One) :. Zero) :. One
--
-- Тоест при pattern matching, ще виждате само най-външния бит
-- ((End :. One) :. Zero) :. One <~ този

-------------------------------------------------------------------------------
-- ЗАДАЧИ --
------------

-- Добавяне на 1 към BitVector
--
-- Примери:
-- >>> succBitVector End
-- End :. One
-- >>> succBitVector (End :. Zero)
-- End :. One
-- >>> succBitVector (End :. One :. One :. One)
-- End :. One :. Zero :. Zero :. Zero
succBitVector :: BitVector -> BitVector
succBitVector = undefined

-- Проверка дали BitVector е числото 0
--
-- Примери:
-- >>> isZero End
-- True
-- >>> isZero (End :. Zero :. Zero)
-- True
-- >>> isZero (End :. One :. Zero)
-- False
-- >>> isZero (End :. One)
-- False
isZero :: BitVector -> Bool
isZero = undefined

-- Казваме че BitVector е в каноничен вид, ако няма водещи нули.
-- Имплементирайте canonicalise, която превръща BitVector в каноничен вид.
--
-- Примери:
-- >>> canonicalise End
-- End
-- >>> canonicalise (End :. One :. Zero)
-- End :. One :. Zero
-- >>> canonicalise (End :. Zero :. Zero :. One :. Zero)
-- End :. One :. Zero
canonicalise :: BitVector -> BitVector
canonicalise = undefined
-- ^ Hint: isZero

-- Конвертирайте число към BitVector (потенциално и в каноничен вид)
--
-- Примери:
-- >>> integerToBitVector 0
-- End
-- >>> integerToBitVector 69
-- End :. One :. Zero :. Zero :. Zero :. One :. Zero :. One
-- >>> integerToBitVector 7
-- End :. One :. One :. One
integerToBitVector :: Integer -> BitVector
integerToBitVector = undefined

-- Конвертирайте BitVector към число
--
-- Примери:
-- >>> bitVectorToInteger  End
-- 0
-- >>> bitVectorToInteger $ End :. One :. Zero :. Zero :. Zero :. Zero
-- 16
bitVectorToInteger :: BitVector -> Integer
bitVectorToInteger = undefined

-- Имплементирайте нужните функции (и инстанции на Bit) за да бъде
-- BitVector инстанция на класовете Eq, Ord и частично на Num.

-- Примери:
-- >>> (+) End (End :. One)
-- End :. One
-- >>> (+) (End :. Zero) End
-- End :. Zero
-- >>> (+) (End :. Zero) (End :. One)
-- End :. One
-- >>> (+) (End :. One) (End :. Zero)
-- End :. One
-- >>> (+) (End :. One :. Zero) (End :. One :. Zero :. Zero)
-- End :. One :. One :. Zero
instance Num BitVector where
  -- Няма нужда да имплементирате долните методи
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

-- >>> (5 :: BitVector) == (5 :: BitVector
-- True
-- >>> (1 :: BitVector) == (5 :: BitVector)
-- False
-- >>> End == (End :. Zero :. Zero)
-- True
instance Eq BitVector
-- ^ Hint: canonicalise


-- >>> compare (5 :: BitVector) (5 :: BitVector
-- EQ
-- >>> compare (17 :: BitVector) (5 :: BitVector)
-- GT
-- >>> compare (5 :: BitVector) (7 :: BitVector)
-- LT
-- >>> compare End (End :. Zero :. Zero)
-- EQ
instance Ord BitVector
-- ^ Hint: canonicalise


-- Имплементирайте mergeSort и го тествайте с BitVector
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined
