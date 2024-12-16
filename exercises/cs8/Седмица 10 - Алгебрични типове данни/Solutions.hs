-- import Prelude hiding (Maybe, Either)

data Type = Product Int Double String | Sum Int Double
newtype Func = Func (Int -> Int)
data Poly t1 t2 = One t1 t2 | Two

-- data Maybe t = Just t | Nothing
-- data Either e v = Left e | Right v

class (Eq t, Show t) => Number t where
    plus :: t -> t -> t
    mult :: t -> t -> t
    fromInt :: Int -> t

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

instance Number Nat where
  plus :: Nat -> Nat -> Nat
  plus Zero n = n
  plus (Succ a) b = Succ $ plus a b

  mult :: Nat -> Nat -> Nat
  mult Zero _ = Zero
  mult (Succ Zero) n = n
  mult (Succ a) b = plus b $ mult a b

  fromInt :: Int -> Nat
  fromInt n
    | n < 0 = fromInt (-n)
    | n == 0 = Zero
    | otherwise = Succ $ fromInt (n - 1)

five :: Nat
five = Succ $ Succ $ Succ $ Succ $ Succ Zero

data Complex = Complex Double Double
  deriving (Eq, Show)

type Complex' = (Double, Double)

-- instance Number Complex where
--   plus :: Complex -> Complex -> Complex
--   plus = _
--   mult :: Complex -> Complex -> Complex
--   mult = _
--   fromInt :: Int -> Complex
--   fromInt = _

data BinaryTree t = Empty | Node t (BinaryTree t) (BinaryTree t)
  deriving Show

instance Functor BinaryTree where  
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap _ Empty = Empty
  fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)

instance Foldable BinaryTree where  
  foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldr _ nv Empty = nv
  foldr op nv (Node root left right) = op root $ foldr op (foldr op nv right) left

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

sub :: Nat -> Nat -> Maybe Nat
sub a b
  | a < b = Nothing
  | a == b = Just Zero
  | otherwise = case (a, b) of
    (a, Zero) -> Just a
    (Succ a, Succ b) -> sub a b

find :: (t -> Bool) -> [t] -> Maybe t
find _ [] = Nothing
find predicate (x:xs) = 
  if predicate x then Just x else find predicate xs

data RegisterForm = RegisterForm String String String String
  deriving Show

data ValidationError = 
  InvalidUsername | 
  InvalidEmail |
  InvalidPassword |
  PasswordsNotMatching
  deriving (Show, Enum)

data User = User String String String

validateUsername :: String -> Either ValidationError String
validateUsername = undefined

validateEmail :: String -> Either ValidationError String
validateEmail = undefined

validatePassword :: String -> Either ValidationError String
validatePassword = undefined

validateMatchingPasswords :: String -> String -> Either ValidationError String
validateMatchingPasswords = undefined

validate :: RegisterForm -> Either ValidationError User
-- Solution using `let` and `pattern matching`
-- validate (RegisterForm username email password repeatPassword) =
--   let validatedUsername = validateUsername username
--       validatedEmail = validateEmail email
--       validatedPassword = validatePassword password
--       validatedPasswords = validateMatchingPasswords password repeatPassword
--    in case (validatedUsername, validatedEmail, validatedPassword, validatedPasswords) of
--         (Left e, _, _, _) -> Left e
--         (_, Left e, _, _) -> Left e
--         (_, _, Left e, _) -> Left e
--         (_, _, _, Left e) -> Left e
--         _ -> Right $ User username email password

-- Solution using `guards`
-- validate (RegisterForm username email password repeatPassword)
--   | isLeft $ validateUsername username = Left InvalidUsername
--   | isLeft $ validateEmail email = Left InvalidEmail
--   | isLeft $ validatePassword password = Left InvalidPassword
--   | isLeft $ validateMatchingPasswords password repeatPassword = Left PasswordsNotMatching
--   | otherwise = Right $ User username email password
--   where isLeft :: Either e v -> Bool
--         isLeft (Left _) = True
--         isLeft _ = False

-- Solution using `do` syntax
validate (RegisterForm username email password repeatPassword) = do
  validatedUsername <- validateUsername username
  validatedEmail <- validateEmail email
  _ <- validatePassword password
  validatedPassword <- validateMatchingPasswords password repeatPassword
  return $ User validatedUsername validatedEmail validatedPassword