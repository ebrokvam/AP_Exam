module CoderImpl where

import Defs
import Control.Monad (ap, liftM)

instance Functor Tree where fmap = liftM
instance Applicative Tree where pure = return; (<*>) = ap

instance Monad Tree where
  return = Found
  Found a >>= f = f a
  Choice [] >>= _ = Choice []
  Choice a >>= f = Choice $ [x >>= f | x <- a]

pick :: [a] -> Tree a
pick [] = Choice []
pick [x] = return x
pick as = Choice $ [pick [x] | x <- as]

solutions :: Tree a -> Int -> Maybe a -> [a]
solutions (Found a) _ _ = [a] 
solutions (Choice []) _ _ = []
solutions (Choice (a:as)) n d = 
  case a of 
    Found x -> 
      if n == 1
        then [x]
        else [x] ++ solutions (Choice as) (n-1) d
    Choice _ -> 
      let l = solutions (Choice as) n d 
          len = length l
      in if len >= n 
        then case d of
          Nothing -> l
          Just d' -> l ++ [d']
        else l ++ solutions a (n-len) d

produce :: [(String,SType)] -> SType -> Tree Exp
produce = undefined

-- recommended, but not mandated, helper function:
extract :: [(String,SType)] -> SType -> SType -> Tree (Exp -> Exp)
extract = undefined