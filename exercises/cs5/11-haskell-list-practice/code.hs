{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- exists :: (a -> Bool) -> [a] -> Bool
-- exists _ [] = False
-- exists p (x:xs) = p x || exists p xs

exists :: (a -> Bool) -> [a] -> Bool
exists p = foldr (\x b -> p x || b) False

member :: Eq a => a -> [a] -> Bool
member x = exists (== x)
