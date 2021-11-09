-- Put your Coder implementation in this file
module CoderImpl where

import Defs
import Control.Monad (ap, liftM)

-- no need to touch these
instance Functor Tree where fmap = liftM
instance Applicative Tree where pure = return; (<*>) = ap

-- data Tree a = Found a | Choice [Tree a]
--  deriving (Eq, Show, Read)
instance Monad Tree where
  return = Found
  Found a >>= f = f a
  Choice [] >>= _ = Choice []
  Choice a >>= f = Choice $ [x >>= f | x <- a]

pick :: [a] -> Tree a
pick = undefined
-- pick (a:as) -> Choice [pick a, pick as]


solutions :: Tree a -> Int -> Maybe a -> [a]
solutions = undefined

produce :: [(String,SType)] -> SType -> Tree Exp
produce = undefined

-- recommended, but not mandated, helper function:
extract :: [(String,SType)] -> SType -> SType -> Tree (Exp -> Exp)
extract = undefined
