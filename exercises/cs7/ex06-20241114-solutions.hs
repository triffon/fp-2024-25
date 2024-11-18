-- Вариант 1: обикновен if-expression, познато
-- Скобите са за указване на приоритет (без тях е неочевидно)
fib n = if n < 2 then n else fib (n-1) + fib (n-2)

-- Вариант 2: алтернативата на cond, наречена guards (гардове, "пазачи")
fib2 n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib2 (n-1) + fib2 (n-2)

-- Вариант 3 (за предпочитане) - pattern matching
fib3 0 = 0
fib3 1 = 1
fib3 n = fib3 (n-1) + fib3 (n-2)

-- Вариант 4 - case-expression - полезни, може би не в този случай
fib4 n = case n of 0 -> 0
                   1 -> 1
                   _ -> fib4 (n-1) + fib4 (n-2)

-- Вариант 5 - итеративна рекурсия с локално дефинирана помощна функция (също познато)
fib5 n = fibHelper n 0 1
  where --fibHelper n curr next
        --  | n == 0    = curr
        --  | otherwise = fibHelper (n-1) next (curr+next)
        -- Бонус: pattern matching и в помощната функция
        fibHelper 0 curr _    = curr
        fibHelper n curr next = fibHelper (n-1) next (curr+next)

-- Зад.3
-- Pattern matching е подходящо за специални стойности,
-- guards за проверка на свойство с функция - но може да се комбинират
fastPow _ 0 = 1
fastPow x n
  | even n    = sq half
  | otherwise = x * sq half
  where sq x = x * x
        half = (fastPow x (div n 2))
