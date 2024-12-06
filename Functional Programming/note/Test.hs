-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, executeCommands, atmChange) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
-- which takes as input a string of bits and checks that

-- the string size is a multiple of 8, and
-- each byte in the string has even parity.

-- The function should return True if both conditions are met, and False
-- otherwise.
-- We are representing bits here by the characters 0 and 1.  You may
-- assume that the input strings contain only 0s and 1s

-- Examples: 
-- checkParity "01010101" = True
-- checkParity "010101011" = False
-- checkParity "010101010" = False
-- checkParity "0101010101010101" = True
-- checkParity "01010101010101010" = False
-- which takes as input a string of bits and checks that

-- the string size is a multiple of 8, and
-- each byte in the string has even parity.

-- The function should return True if both conditions are met, and False
-- otherwise.
-- We are representing bits here by the characters 0 and 1.  You may
-- assume that the input strings contain only 0s and 1s
    checkParity :: String -> Bool
    checkParity n  = (length n `mod` 8 == 0) && (all even (map (length . filter (=='1')) (splitEvery 8 n)))

    splitEvery :: Int -> [a] -> [[a]]
    splitEvery _ [] = []
    splitEvery n xs = take n xs : splitEvery n (drop n xs)

{- Question 2 -}
{- Question 2 -}
-- Examples

-- key1 :: String
-- key1 = "LYKBDOCAWITNVRHJXPUMZSGEQF"

-- key2 :: String
-- key2 = "UDMZIQKLNJOSVETCYPBXAWRGHF"

-- plaintext1 :: String
-- plaintext1 = "The Quick Brown Fox Jumped Over The Lazy Dog"

-- plaintext2 :: String
-- plaintext2 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

-- ghci> substitution plaintext1 key1
-- "Mad Xzwkt Yphgr Ohe Izvjdb Hsdp Mad Nlfq Bhc"

-- ghci> substitution plaintext1 key2
-- "Xli Yanmo Dptre Qtg Javciz Twip Xli Sufh Ztk"

-- ghci> substitution plaintext2 key1
-- "Nhpdv wjuzv bhnhp uwm lvdm, khrudkmdmzp lbwjwukwrc dnwm, udb bh dwzuvhb mdvjhp wrkwbwbzrm zm nlyhpd dm bhnhpd vlcrl lnwxzl. Zm drwv lb vwrwv sdrwlv, xzwu rhumpzb dedpkwmlmwhr znnlvkh nlyhpwu rwuw zm lnwxzwj de dl khvvhbh khrudxzlm. Bzwu lzmd wpzpd bhnhp wr pdjpdadrbdpwm wr shnzjmlmd sdnwm duud kwnnzv bhnhpd dz ozcwlm rznnl jlpwlmzp. Dekdjmdzp uwrm hkkldklm kzjwblmlm rhr jphwbdrm, uzrm wr kznjl xzw hoowkwl bdudpzrm vhnnwm lrwv wb dum nlyhpzv."
charLabel :: Char -> Int
charLabel char =  ord (toUpper char) - ord 'A'

substitution :: String -> String -> String
substitution plaintext key = map (substitutionChar key) plaintext

substitutionChar :: String -> Char -> Char
substitutionChar key char
  | isAlpha char = chr (ord 'A' + (charLabel char + charLabel (key !! charLabel char)) `mod` 26)
  | otherwise = char


-- Background Material (Part 1 - [5 out of 10 marks])
-- A famous theorem about prime numbers (called Chebyshev's Theorem) asserts that
-- for any number n, there always exists a prime number p such that n < p < 2n. That is, there is always a prime number between n and 2n.

-- Implementation Task
-- Write a function

-- which returns the largest prime between n and 2n.

-- Examples

-- ghci> largestPrimeBetween 4
-- 7
-- ghci> largestPrimeBetween 10
-- 19

largestPrimeBetween :: Int -> Int
largestPrimeBetween n = last (takeWhile (<= 2*n) (filter isPrime [n..]))


-- Background Material
-- In number theory, a strong prime is a prime number that is greater than the
-- average of the nearest prime above and below. In other words, it is closer to
-- the succeeding prime than it is to the preceding one.
-- For example, 17 is the seventh prime: the sixth and eighth primes, 13 and 19,
-- add up to 32, and half of that is 16; 17 is greater than 16, so 17 is a strong
-- prime.


-- ghci> strongPrimes 25
-- [11,17,29,37,41,59,67,71,79,97,101,107,127,137,149,163,179,191,197,223,227,239,251,269,277]
-- factors :: Int -> [Int]
-- factors n = [ k | k <- [1..n] , n `mod` k == 0 ]

-- isPrime :: Int -> Bool
-- isPrime n = factors n == [1, n]
-- which takes as input the integer n and prints the first n strong prime
-- numbers.

strongPrimes :: Int -> [Int]
strongPrimes n = take n [k | k <- [1..], isStrongPrime k]

isStrongPrime :: Int -> Bool
isStrongPrime n = isPrime n && n > (average (nearestPrimes n))

nearestPrimes :: Int -> [Int]
nearestPrimes n = [k | k <- [1..], isPrime k, k < n] ++ [k | k <- [1..], isPrime k, k > n]

average :: [Int] -> Int
average xs = sum xs `div` length xs





{- Question 4 -}
-- Background Material
-- Consider the following data type of directions

-- data Direction = MoveLeft
--                | MoveRight
--                | MoveUp 
--                | MoveDown
--                deriving (Eq, Show)


-- Let us define the type Command to consist of a pair of a Direction and an
-- Int.

-- type Command = (Direction, Int)


-- Given a coordinate pair (x, y), the execution of a command consists in
-- incrementing the corresponding coordinate.
-- So for example, executing (MoveLeft, 10) on the pair (5, 5) should result in
-- (-5, 5). (We use the mathematical indexing: "right" means increasing the x
-- coordinate and "up" means increasing the y coordinate).

-- Implementation Task
-- Write a function which, given an initial position (x, y), computes the final
-- position after the execution of a list of commands.

-- executeCommands :: [Command] -> (Int , Int) -> (Int , Int)
-- executeCommands = undefined



-- Examples

-- ghci> executeCommands [(MoveRight,10),(MoveLeft,5),(MoveUp,20)] (0,0)
-- (5,20)

executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands [] (x, y) = (x, y)
executeCommands ((MoveLeft, n):xs) (x, y) = executeCommands xs (x - n, y)
executeCommands ((MoveRight, n):xs) (x, y) = executeCommands xs (x + n, y)
executeCommands ((MoveUp, n):xs) (x, y) = executeCommands xs (x, y + n)
executeCommands ((MoveDown, n):xs) (x, y) = executeCommands xs (x, y - n)

-- Background Material

{- Question 5 -}
-- ghci> atmChange 2180 [10, 50, 100, 500]
-- [(500,4),(100,1),(50,1),(10,3)]

-- ghci> atmChange 3270 [10, 50, 100, 500]
-- [(500,6),(100,2),(50,1),(10,2)]

-- ghci> atmChange 3270 [10, 50, 100, 200, 500]
-- [(500,6),(200,1),(100,0),(50,1),(10,2)]

-- ghci> atmChange 3275 [1, 20, 50, 100, 500, 1000]
-- [(1000,3),(500,0),(100,2),(50,1),(20,1),(1,5)]

-- In the above example, the user has asked for £2180 with allowed denominations of
-- 10, 50, 100, and 500. The algorithm computes that the ATM will need to dispense
-- 4 bills of 500, 1 bill of 100, 1 bill of 50 and 3 bills of 10.
-- Note that 2180 = 4 x 500 + 1 x 100 + 1 x 50 + 3 x 10
-- Some more examples follow.
-- For amount 3600 and list of denominations [1200], your implementation of 'atmChange' returned [(1200,1),(1200,1),(1200,1)] but the correct answer is [(1200,3)].
-- which takes an amount and a list of denominations in ascending order (such as
-- [10, 20, 50, 100]) and returns a list of pairs of denominations and number of
-- notes to be dispensed (with the fewest number of notes).
atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange amount denominations = atmChange' amount denominations []

atmChange' :: Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
atmChange' 0 _ acc = acc
atmChange' amount (d:ds) acc
  | amount >= d = atmChange' (amount - d) (d:ds) ((d, 1):acc)
  | otherwise = atmChange' amount ds acc




-- Background Material






