-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

import System.Random
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List

import Week10Extra

--
--  Practicing with Type Constuctors, Functors and Monads
--

type F1 a = Maybe a
type F2 a = Either a String
type F3 a = [ a ]
type F4 a = BinT a
type F5 a = Int -> a
type F6 a = (a -> Int) -> Int
type F7 a = RoseT a
type F8 a = ThreeT a
type F9 a = Expr a 

-- fmap :: (a -> b) -> f a -> f b
fmap1 :: (a -> b) -> F1 a -> F1 b
fmap1 f (Just x) = Just (f x)
fmap1 f Nothing = Nothing

fmap2 :: (a -> b) -> F2 a -> F2 b
fmap2 f (Left x) = Left (f x)
fmap2 f (Right x) = Right x


fmap3 :: (a -> b) -> F3 a -> F3 b
fmap3 f [] = []
fmap3 f (x:xs) = (f x) : (fmap3 f xs)

fmap4 :: (a -> b) -> F4 a -> F4 b
fmap4 f (Leaf x) = Leaf (f x)
fmap4 f (Fork l x r) = Fork (fmap4 f l) (f x) (fmap4 f r)

fmap5 :: (a -> b) -> F5 a -> F5 b
fmap5 f g = f . g

fmap6 :: (a -> b) -> F6 a -> F6 b 
fmap6 f g k = f (g k)

fmap7 :: (a -> b) -> F7 a -> F7 b
fmap7 f (Rose x []) = Rose (f x) []
fmap7 f (Rose x xs) = Rose (f x) (map (fmap7 f) xs)
    

fmap8 :: (a -> b) -> F8 a -> F8 b
fmap8 = undefined

fmap9 :: (a -> b) -> F9 a -> F9 b
fmap9 = undefined

-- pure :: a -> f a

pure1 :: a -> F1 a
pure1 = undefined

pure2 :: a -> F2 a
pure2 = undefined

pure3 :: a -> F3 a
pure3 = undefined

pure5 :: a -> F5 a
pure5 = undefined

pure6 :: a -> F6 a
pure6 = undefined

pure9 :: a -> F9 a
pure9 = undefined

-- (>>=) :: f a -> (a -> f b) -> f b

bind1 :: F1 a -> (a -> F1 b) -> F1 b
bind1 = undefined

bind2 :: F2 a -> (a -> F2 b) -> F2 b
bind2 = undefined

bind3 :: F3 a -> (a -> F3 b) -> F3 b
bind3 = undefined

bind5 :: F5 a -> (a -> F5 b) -> F5 b
bind5 = undefined

bind6 :: F6 a -> (a -> F6 b) -> F6 b
bind6 = undefined

bind9 :: F9 a -> (a -> F9 b) -> F9 b
bind9 = undefined

--
--  Using Monads
--

labelRoseS :: Rose a -> State Int (Rose (Int,a))
labelRoseS = undefined

labelRose :: Rose a -> Rose (Int,a)
labelRose = undefined

readAndRespond :: IO ()
readAndRespond = undefined

flipWords :: IO ()
flipWords = undefined

--
--  Using the Picking Monad
--

choose :: PickingMonad m => [a] -> m a
choose = undefined

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate = undefined

cut :: PickingMonad m => [a] -> m ([a],[a])
cut = undefined

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle = undefined

riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles = undefined

permute :: PickingMonad m => [a] -> m [a]
permute = undefined

genTree :: PickingMonad m => [a] -> m (Bin a)
genTree = undefined



