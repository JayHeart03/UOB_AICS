-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
module ClassTest2 (dec2FactString , factString2Dec , treeByLevels, rotateLeft, rotateRight, leafIndices, treeToParens, parensToTree, phoneToString, stringToPhone, fingerTaps, facHelper, factorial', applyfuns, updateNodes, eval, run) where

import Types

import Data.Char
import Data.List

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Exercise 1
-- Implementation Task
-- Coding decimal numbers with factorials is a way of writing out numbers in a base system that depends on factorials, rather than powers of numbers.

-- In this system, the last digit is always 0 and is in base 0!. The digit before that is either 0 or 1 and is in base 1!. The digit before that is either 0, 1, or 2 and is in base 2!, etc. More generally, the nth-to-last digit is always 0, 1, 2, ..., n and is in base n!.

-- Read more about it at: http://en.wikipedia.org/wiki/Factorial_number_system

-- Example
-- The decimal number 463 is encoded as "341010", because:

-- 463 = 3×5! + 4×4! + 1×3! + 0×2! + 1×1! + 0×0!

-- If we are limited to digits 0..9, the biggest number we can encode is 10!-1 (= 3628799). So we extend 0..9 with letters A..Z. With these 36 digits we can now encode numbers up to 36!-1 (= 3.72 × 1041)

-- Task
-- We will need two functions. The first one will receive a decimal number and return a string with the factorial representation.

-- The second one will receive a string with a factorial representation and produce the decimal representation.

-- Given numbers will always be positive.

-- I defined factorial for you

-- factorial :: Integer -> Integer
-- factorial 0 = 1
-- factorial n = n * factorial (n-1)
-- Hint: You can use toIntegral to translate Int to Integer
import Data.Char

dec2FactString :: Integer -> String
dec2FactString n = reverse $ dec2FactString' n 0

dec2FactString' :: Integer -> Int -> String
dec2FactString' 0 _ = []
dec2FactString' n i = (toEnum $ fromIntegral $ n `mod` (toInteger $ i + 1) + 48) : dec2FactString' (n `div` (toInteger $ i + 1)) (i + 1)


factString2Dec :: String -> Integer 
factString2Dec str = factString2Dec' (reverse str) 0

factString2Dec' :: String -> Int -> Integer
factString2Dec' [] _ = 0
factString2Dec' (x:xs) i = (toInteger $ fromEnum x - 48) * (factorial $ toInteger $ i) + factString2Dec' xs (i + 1)


-- Exercise 2

-- You are given a binary tree:

-- data TreeNode a = TreeNode {
--   left  :: Maybe (TreeNode a),
--   right :: Maybe (TreeNode a),
--   value :: a
--   } deriving Show


-- Your task is to return the list with elements from tree sorted by levels, which means the root element goes first, then root children (from left to right) are second and third, and so on.

-- Return empty list if root is Nothing.

-- Example 1 - following tree:

--                  2
--             8        9
--           1  3     4   5
-- Should return following list:

-- [2,8,9,1,3,4,5]

-- Example 2 - following tree:

--                  1
--             8        4
--               3        5
--                          7
-- Should return following list:

-- [1,8,4,3,5,7]

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels (Just (TreeNode l r v)) = v : treeByLevels l ++ treeByLevels r

-- In this Kata you will rotate a binary tree. You need to implement two methods to rotate a binary tree: one to rotate it to the left and one to rotate it to the right.

-- If rotation is impossible, return the tree unchanged.

-- Tree structure
-- data Tree a = Empty
--             | Node { left', right' :: Tree a , value' :: a }
--             deriving (Show,Eq,Foldable)
-- What is a binary tree?
-- A binary tree is a tree graph, in which each element can't have more than 2 children. Values can not be duplicated, so (sub)trees can be associated with, and denoted by, their value.

-- What does rotate mean?
-- What does it mean to rotate a binary tree in this case? The rotation changes the root of the tree to be the left or right child of the current root. For example:


--       9
--      / \
--     7   11
--    / \
--   5   8
-- In this case the root is 9, its left child is 7, and its right child is 11.

-- If we rotate it to the right, we'll get this tree:

--     7
--    / \
--   5   9
--      / \
--     8   11
-- We move the left child of the old root (7) to be the new root, and move its right child (8) to be the new left child of the old root.

-- If we rotate it to the left, we'll get another tree:

--        11
--        /
--       9
--      /
--     7
--    / \
--   5   8
-- We move the right child of the old root (11) to be the new root, and move its left child (null in this case) to be the new right child of the old root.



rotateLeft,rotateRight :: Tree a -> Tree a

rotateRight Empty = Empty
rotateRight (Node Empty r v) = Node Empty r v
rotateRight (Node (Node l' r' v') r v) = Node l' (Node r' r v) v'

rotateLeft Empty = Empty
rotateLeft (Node l Empty v) = Node l Empty v
rotateLeft (Node l (Node l' r' v') v) = Node (Node l l' v) r' v'


-- Exercise 4
-- data BT a = Empty' | Fork a (BT a) (BT a)

-- Given a binary tree, let us label the leaves from left to right starting at 0. Each node then determines a pair of integers (i,j) where i is the index of its left-most leaf and j is the index of its rightmost leaf. Write a function:

-- Which replaces each node with the pair (i,j) of indices of its leftand right-most leaves. For example, the tree:

-- 	   a
-- 	  /  \
-- 	 b    c
-- 	/ \  / \ 
-- 	        d
-- 	       / \
-- would be mapped to the tree

-- 	  (0,4)
-- 	  /    \
-- 	(0,1)  (2,4)
-- 	 / \   /   \ 
-- 	          (3,4)
-- 	          /   \

leafIndices :: BT a -> BT (Int,Int)
leafIndices Empty' = Empty'
leafIndices (Fork a l r) = Fork (leafIndices' l 0, leafIndices' r 0) (leafIndices l) (leafIndices r)

leafIndices' :: BT a -> Int -> Int
leafIndices' Empty' i = i
leafIndices' (Fork a l r) i = leafIndices' r (i + 1)


-- Exercise 5
-- Exercise 5 - Trees to Parentheses, and Back
-- Binary trees can be encoded as strings of balanced parentheses (in fact, the two things are isomorphic). Your task is to figure out such an encoding, and write the two functions which convert back and forth between the binary trees and strings of parentheses.

-- Here's the definition of binary trees:

-- data Tree' = Leaf | Tree' :*: Tree' deriving (Eq, Show)
-- And here are the functions you need to define:

-- treeToParens :: Tree -> String
-- parensToTree :: String -> Tree
-- The first function needs to return only strings of valid balanced parentheses (like "()(())"). The second needs to accept any string of balanced parentheses.

-- Also, the functions need to be inverses of each other. In other words, they need to satisfy the following equations:

-- forall s. treeToParens (parensToTree s) = s
-- forall t. parensToTree (treeToParens t) = t

treeToParens :: Tree -> String
treeToParens Leaf = ""
treeToParens (l :*: r) = "(" ++ treeToParens l ++ ")" ++ "(" ++ treeToParens r ++ ")"


parensToTree :: String -> Tree
parensToTree "" = Leaf
parensToTree s = fst $ iter s
    where iter :: String -> (Tree, String)
          iter ('(':xs) = let (l, ys) = iter xs
                              (r, zs) = iter ys
                          in (l :*: r, zs)
          iter (')':xs) = (Leaf, xs)
          iter (_:xs) = iter xs
         iter s = (Leaf, tail s)


-- Exercise 6

{- Question 6a -}
phoneToString :: [(Button, Presses)] -> Text
phoneToString = undefined

{- Question 6b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined

{- Question 6c -}
fingerTaps :: Text -> Presses
fingerTaps = undefined

-- Exercise 7

facHelper :: Integer -> State Integer ()
facHelper = undefined

factorial' :: Integer -> Integer
factorial' n = snd (runState (facHelper n) 1)

-- Exercise 8

applyfuns :: (a -> c) -> (b -> d) -> Tree'' a b -> Tree'' c d
applyfuns = undefined

-- Exercise 9

updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes = undefined

-- Exercise 10

eval :: MonadError String m => CalcExpr -> m Int
eval = undefined

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run = undefined