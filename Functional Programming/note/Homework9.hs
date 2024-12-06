-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}

module Homework9 (applyfuns, updateNodes, eval, run) where

import Types

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Question 0
-- Consider the following Java method:

-- int fac (int n) {
--   int y = 1;
--   while (n > 1} {
--    System.out.println("n = " + n);
--    y = y * n;
--    n--;
--    }
--   }
--   return y;


-- Write this as a Haskell function using the state modify monad:
factorial :: Integer -> Integer
factorial n = snd (runState (facHelper n) 1)

facHelper :: Integer -> State Integer ()
facHelper 0 = return ()
facHelper n |
    n > 1 = do
        modify (*n)
        facHelper (n-1)
    | otherwise = return ()



-- Question 1

-- Background Material
-- There are many possible variants of the type of binary trees
-- introduced in the Lecture Notes.  For example, consider the following
-- definition:

-- data Tree a b = Leaf b | Fork (Tree a b) a (Tree a b)
--   deriving (Eq, Show)


-- Notice how now, the tree stores two different types
-- of data: an element of type a at each fork and an element of type
-- b at each leaf.

-- Implementation Task
-- Your task is to write a higher-order function applyfuns that takes
-- two functions f :: a -> b and g :: b -> d, as well as an element
-- of type Tree a b as input and applies the first function to the
-- values found at the forks, and the second function to the values found
-- at the leaves.  That is, implement the function:

-- Examples
-- Lets consider the following two functions:

-- str2int :: String -> Int
-- str2int xs = length xs

-- int2bool :: Int -> Bool
-- int2bool n = n /= 0


-- and the following binary tree:

--        "John"
--        /    \
--       /      \
--   "Oliver" "Benjamin"
--    /   \      /   \
--   /     \    /     \
--  2       4  0       6


-- Then the expression applyfuns str2int int2bool should return the tree

--           4
--        /    \
--       /      \
--      6          8
--    /   \      /   \
--   /     \    /     \
-- True   True False  True



-- *Main> applyfuns str2int int2bool (Fork (Fork (Leaf 2) "Oliver" (Leaf 4)) "John" (Fork (Leaf 0) "Benjamin" (Leaf 6)))
-- Fork (Fork (Leaf True) 6 (Leaf True)) 4 (Fork (Leaf False) 8 (Leaf True))



-- As a second example, the tree

--                    "New York"
--                     /      \
--                    /        \
--               "Paris"      "Dubai"
--               /    \	   /    \
--              /      \     /      \
--         "London"    14   5    "Shanghai"
--            /   \                /     \
--           /     \              /       \
--          0      10            0        21


-- is transformed into the tree

--                        8
--                     /      \
--                    /        \
--                  5           5
--               /    \	   /    \
--              /      \     /      \
--              6     True True       8
--            /   \                /     \
--           /     \              /       \
--         False  True          False     True

-- Implementation Task
-- Your task is to write a higher-order function applyfuns that takes
-- two functions f :: a -> b and g :: b -> d, as well as an element
-- of type Tree a b as input and applies the first function to the
-- values found at the forks, and the second function to the values found
-- at the leaves.  That is, implement the function:
-- data Tree a b = Leaf b | Fork (Tree a b) a (Tree a b)
--   deriving (Eq, Show)

-- str2int :: String -> Int
-- str2int xs = length xs

-- int2bool :: Int -> Bool
-- int2bool n = n /= 0
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns f g (Leaf b) = Leaf (g b)
applyfuns f g (Fork l a r) = Fork (applyfuns f g l) (f a) (applyfuns f g r)



-- Question 2
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes = undefined

-- Question 3
eval :: MonadError String m => CalcExpr -> m Int
eval = undefined

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run = undefined
