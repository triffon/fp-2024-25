-- allows us to write signatures in instance declarations
{-# LANGUAGE InstanceSigs #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- no incomplete patterns in lambdas!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

-- Bitvectors:
--------------
-- Ще си дефинираме битове.
-- На практика Bit е същото като Bool, но ще го използваме в друг контекст.
data Bit = Zero | One
  deriving (Show, Eq)

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
--
-- Проверка за празен вектор:
isEnd :: BitVector -> Bool
isEnd End = True
isEnd (_ :. _) = False

-- Number: 6
-- Binary: 110
-- BitVector: End :. One :. One :. Zero
--
-- Number: 5
-- Binary: 101
-- BitVector: End :. One :. Zero :. One

-- Изместване с бит надясно:
--
-- 1101 -> 110
-- >>> shiftRight (End :. One :. One :. Zero :. One)
-- ((End :. One) :. One) :. Zero
shiftRight :: BitVector -> BitVector
shiftRight End = End
shiftRight (bv :. _) = bv

-- Не забравяйте че (:.) е дясно асоциативна:
-- >>> End :. One :. Zero :. One
-- ((End :. One) :. Zero) :. One
--
-- Тоест при pattern matching, ще виждате само най-външния бит
-- ((End :. One) :. Zero) :. One <- този

-------------------------------------------------------------------------------
-- ЗАДАЧИ --
------------
succBitVector :: BitVector -> BitVector
succBitVector End = End :. One
succBitVector (bv :. Zero) = bv :. One
succBitVector (bv :. One) = succBitVector bv :. Zero

isZero :: BitVector -> Bool
isZero End = True
isZero (bv :. Zero) = isZero bv
isZero (_ :. One) = False

-- TODO: The case (End :. Zero) may be a problem for (+), Eq or Ord
canonicalise :: BitVector -> BitVector
canonicalise End = End
canonicalise (bv :. bit)
  | isZero bv = End :. bit
  | otherwise = canonicalise bv :. bit

bitVectorToInteger :: BitVector -> Integer
bitVectorToInteger = bvToInt 0 . canonicalise
  where
    bvToInt _ End = 0
    bvToInt n (bv :. One) = 2 ^ n + bvToInt (n + 1) bv
    bvToInt n (bv :. Zero) = bvToInt (n + 1) bv

instance Num BitVector where
  fromInteger :: Integer -> BitVector
  fromInteger 0 = End
  fromInteger n
    | even n = rec Zero
    | otherwise = rec One
    where
      rec bit = fromInteger (n `div` 2) :. bit

  -- Тук скобите са малко коварни
  (+) :: BitVector -> BitVector -> BitVector
  End + bv = bv
  bv + End = bv
  (bv1 :. bit1) + (bv2 :. bit2) =
    case (bit1, bit2) of
      (Zero, Zero) -> bv1 + (bv2 :. Zero)
      (One, One) -> (succBitVector bv1 + bv2) :. Zero
      _ -> bv1 + (bv2 :. One)

  -- Няма нужда да имплементирате долните
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

-- Изберете един да реализирате.
-- Другият имплементирайте чрез избрания.
instance Eq BitVector where
  (==) :: BitVector -> BitVector -> Bool
  bv1 == bv2 = eq cbv1 cbv2
    where
      cbv1 = canonicalise bv1
      cbv2 = canonicalise bv2
      -- Вложена дефиниция, за да обходим рекурсивно канонизираните вектори
      eq End End = True
      eq (bv1' :. bit1) (bv2' :. bit2) = bit1 == bit2 && eq bv1' bv2'
      eq _ _ = False

  -- Друг вариант е да канонизираме на всяка стъпка
  (/=) :: BitVector -> BitVector -> Bool
  bv1 /= bv2
    | isZero bv1 && isZero bv2 = False
    | otherwise = case (bv1', bv2') of
        (bv1'' :. One, bv2'' :. Zero) -> bv1'' /= bv2''
        (bv1'' :. Zero, bv2'' :. One) -> bv1'' /= bv2''
        _ -> False
    where
      bv1' = canonicalise bv1
      bv2' = canonicalise bv2

instance Ord Bit where
  compare :: Bit -> Bit -> Ordering
  compare Zero One = LT
  compare One Zero = GT
  compare _ _ = EQ

-- Не съм сигурен как би се имплементирало (<=)
instance Ord BitVector where
  compare :: BitVector -> BitVector -> Ordering
  compare bv1 bv2 = compare' cbv1 cbv2
    where
      cbv1 = canonicalise bv1
      cbv2 = canonicalise bv2
      compare' :: BitVector -> BitVector -> Ordering
      compare' End End = EQ
      compare' End _ = LT
      compare' _ End = GT
      compare' (bv1' :. b1) (bv2' :. b2) =
        let rec = compare' bv1' bv2'
         in case rec of
              EQ -> compare b1 b2
              _ -> rec

-- Имплементирайте mergeSort и го тествайте с BitVector
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort l1) (mergeSort l2)
  where
    (l1, l2) = splitAt (length xs `div` 2) xs
