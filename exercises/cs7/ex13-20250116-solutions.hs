import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad.State

-- Зад.1
data Expr = Const Double
          | Var
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Exp Expr Int
          deriving Show

eval :: Expr -> Double -> Double
eval (Const n) _ = n
eval (Var) x = x
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Sub e1 e2) x = eval e1 x - eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Exp e1 n) x = (eval e1 x) ^ n

derive :: Expr -> Expr
derive (Const _) = Const 0
derive (Var) = Const 1
derive (Add e1 e2) = Add (derive e1) (derive e2)
derive (Sub e1 e2) = Sub (derive e1) (derive e2)
derive (Mul e1 e2) = Add (Mul (derive e1) e2) (Mul e1 (derive e2))
derive (Exp e1 n)
  | n > 0     = Mul (Mul (Const (fromIntegral n)) (Exp e1 (n-1))) (derive e1)
  | otherwise = error "to-do"

e :: Expr
e = Exp (Add (Mul (Const 2) Var) (Const 1)) 3

de :: Expr
de = derive e

-- Можем да разпознаваме и елиминираме конкретни изрази
arithmSimplify :: Expr -> Expr
-- много дъна на рекурсията
arithmSimplify (Mul (Const 0) _) = Const 0
arithmSimplify (Mul _ (Const 0)) = Const 0
arithmSimplify (Mul (Const 1) e) = e
arithmSimplify (Mul e (Const 1)) = e
arithmSimplify (Add (Const 0) e) = e
arithmSimplify (Add e (Const 0)) = e
arithmSimplify (Exp e 1) = e
arithmSimplify (Exp e 0) = Const 1
-- Общите случаи
-- Проблем: може би трябва да разпознаем дали след рекурсивните прилагания
-- се получава нещо тип (Add (Const 0) _), т.е. подлежи на още една стъпка опростяване
-- => трябват отделни функции за едностъпково и многостъпково опростяване :(
arithmSimplify (Add e1 e2) = Add (arithmSimplify e1) (arithmSimplify e2)
arithmSimplify (Sub e1 e2) = Sub (arithmSimplify e1) (arithmSimplify e2)
arithmSimplify (Mul e1 e2) = Mul (arithmSimplify e1) (arithmSimplify e2)
arithmSimplify (Exp e1 n) = Exp (arithmSimplify e1) n
arithmSimplify e = e

-- Constant folding, или елиминиране на цели дървета (подизрази),
-- в които има само константи.
constantFold :: Expr -> Expr
constantFold (Add e1 e2) = case (constantFold e1, constantFold e2)
                           of (Const c1, Const c2) -> Const (c1 + c2)
                              (e1', e2') -> Add e1' e2'
constantFold (Sub e1 e2) = case (constantFold e1, constantFold e2)
                           of (Const c1, Const c2) -> Const (c1 - c2)
                              (e1', e2') -> Sub e1' e2'
constantFold (Mul e1 e2) = case (constantFold e1, constantFold e2)
                           of (Const c1, Const c2) -> Const (c1 * c2)
                              (e1', e2') -> Mul e1' e2'
constantFold (Exp e1 n) = case constantFold e1
                          of Const c1 -> Const (c1 ^ n)
                             e1' -> Exp e1' n
constantFold e = e

-- Редът на оптимизациите може да има значение
simplify :: Expr -> Expr
simplify = constantFold . arithmSimplify

-- Зад.2
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

values :: Tree a -> [a]
values Empty = []
values (Node val l r) = values l ++ [val] ++ values r

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (x:xs) = x : uniques (filter (/=x) xs)

-- Хубава имплементация, но с две обхождания на дървото
labelTree :: Eq a => Tree a -> Tree Int
labelTree t = fmap foo t
  where lst = uniques $ values t
        foo x = succ $ fromJust $ elemIndex x lst

labelTree' :: Eq a => Tree a -> Tree Int
labelTree' t = fst $ helper t []
  where -- Приема текущият списък на срещнати и връща полученият такъв след обхождане на дървото
        -- Заб.: ръчното предаване на правилната "версия" на състоянието е неудобна и лесна за объркване
        helper :: Eq a => Tree a -> [a] -> (Tree Int, [a])
        helper Empty lst = (Empty, lst)
        helper (Node val l r) lst = (Node idx l' r', lst3)
          where (l', lst1) = helper l lst
                lst2 = if val `elem` lst1 then lst1 else lst1++[val]
                idx = succ $ fromJust $ elemIndex val lst2
                (r', lst3) = helper r lst2

-- Функцията е чиста, изпозлването на State монада е като имплементационен детайл.
-- По дадено първоначално състояние цялото изчисление е детерминистично, няма нищо "мръсно" като при IO.
-- Монадът е само за удобство :)
labelTreeS :: Eq a => Tree a -> Tree Int
labelTreeS t = evalState (helperS t) []
  where -- Монадът се грижи за поддържане на "актуалната версия" на състоянието
        helperS :: Eq a => Tree a -> State [a] (Tree Int)
        helperS Empty = return Empty -- "опаковане" на чиста стойност
        helperS (Node val l r) = do
            l' <- helperS l
            modify (\lst -> if val `elem` lst then lst else lst++[val])
            idx <- gets (\lst -> succ $ fromJust $ elemIndex val lst)
            r' <- helperS r
            return (Node idx l' r')

t :: Tree Char
t = Node 'c' (Node 'a' (Node 'b' Empty Empty)
                       (Node 'a' Empty Empty))
             (Node 'b' Empty Empty)

-- Пример за списъкът като монад, който "генерира" стойности недетерминистично.
-- За всяка стойност, генерирана от даден списък, се извикват останалите "изчисления"
f :: [Int]
f = do x <- [1,2,3]
       y <- [4,5]
       return (x+y)
-- Пример за Maybe като монад, който може да успее да генерира стойност, може и да не успее.
-- Ако някое по-ранно изчисление на успее, следващите не се изпълняват и се връща Nothing
g :: Bool -> Char -> Maybe (Int,Int)
g x y = do i <- elemIndex x [True,False]
           j <- elemIndex y "abcdef"
           return (i,j)
-- Аналогично, State е монад, при който генерирането на стойност зависи от предварително зададено "състояние".
-- Състоянието, получено след генериране на дадена стойност, се използва като първоначално за следващите изчисления
