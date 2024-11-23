-- Write a function to pair each element of a list with its index.
withIndex :: [a] -> [(Int,a)]
withIndex l = zip [0..] l

-- Using guarded equations, write a function of type Int -> Int -> Bool that returns True if the first argument is greater than the second and less than twice the second.
betweenNand2n :: Int -> Int -> Bool
betweenNand2n x y
  | x > y && x < 2*y = True
  | otherwise = False

-- Select the nth element of a list:
(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "Index out of bounds"
(!!!) (x:xs) n
  | n == 0 = x
  | otherwise = xs !!! (n-1)


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- A triple (x,y,z) of positive integers is called pythagorean if
-- x^2 + y^2 = z^2 . Using a list comprehension, define a function:
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Express the comprehension [f x | x <- xs, p x] using the functions map and filter. The function type is given as:
fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
fun f p xs = map f (filter p xs)

-- Redefine map f and filter p using foldr and foldl. For your reference, here are the definitions of map and filter from lecture notes. HINT. Read about the foldr and foldl functions in the handout higher-order functions and Chapter 7.3 and 7.4 of Programming in Haskell.

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
   | p x       = x : filter p xs
   | otherwise = filter p xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternatively applies the two argument functions to successive elements in a list.
For example:

> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- which takes as input a string of bits and checks that

-- the string size is a multiple of 8, and
-- each byte in the string has even parity.

-- The function should return True if both conditions are met, and False
-- otherwise.
-- We are representing bits here by the characters 0 and 1.  You may
-- assume that the input strings contain only 0s and 1s.

checkParity :: String -> Bool
checkParity s = (length s `mod` 8 == 0) && (all evenParity (chunksOf 8 s))

evenParity :: String -> Bool
evenParity s = even (length (filter (=='1') s))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)



-- A substitution cipher is an old method of encryption, in which the cipher
-- takes a string and a key that is as long as the alphabet that the message
-- uses. In our case, the message will be expressed using the English alphabet so
-- our cipher key will be a string of length 26. This represents a mapping of each
-- letter of the alphabet to a different letter.
-- For example, the key "LYKBDOCAWITNVRHJXPUMZSGEQF" maps 'A' to 'L',
-- 'B' to 'Y', 'C' to 'K' and so on.

-- which takes a plaintext string (that might contain punctuation and spaces) and
-- an uppercase key and returns the ciphertext.
-- Note the following:

-- The capitalisation of the characters in the plaintext must be preserved by
-- your implementation.
-- The encryption should apply only to the letters (i.e. the alphabetic
-- characters) and punctuation and spaces should be ignored. For this purpose,
-- you can use the isLetter :: Char -> Bool function coming from Data.Char to
-- test if a given character is a letter.
-- You may wish to use the function


charLabel :: Char -> Int
charLabel char =  ord (toUpper char) - ord 'A'

substitution :: String -> String -> String
substitution plaintext key = map (\c -> if isLetter c then getnewletter c key else c) plaintext

getnewletter :: Char -> String -> Char
getnewletter c key = if isUpper c then toUpper (key !! charLabel c) else toLower (key !! charLabel c)

-- A famous theorem about prime numbers (called Chebyshev's Theorem) asserts that
-- for any number n, there always exists a prime number p such that n < p < 2n. That is, there is always a prime number between n and 2n.

-- Implementation Task
-- Write a function
largestPrimeBetween :: Int -> Int
largestPrimeBetween n = last (takeWhile (<= 2*n) (filter isPrime [n..]))

-- Background Material (Part 2 - [5 out of 10 marks])
-- In number theory, a strong prime is a prime number that is greater than the
-- average of the nearest prime above and below. In other words, it is closer to
-- the succeeding prime than it is to the preceding one.
-- For example, 17 is the seventh prime: the sixth and eighth primes, 13 and 19,
-- add up to 32, and half of that is 16; 17 is greater than 16, so 17 is a strong
-- prime.

-- which takes as input the integer n and prints the first n strong prime
-- numbers.

-- Examples

-- ghci> strongPrimes 25
-- [11,17,29,37,41,59,67,71,79,97,101,107,127,137,149,163,179,191,197,223,227,239,251,269,277]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

strongPrimes :: Int -> [Int]
strongPrimes n = take n [x | x <- [3..],isPrime x && isStrongPrime x]

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

executeCommands :: [Command] -> (Int , Int) -> (Int , Int)
executeCommands [] (x,y) = (x,y)
executeCommands ((MoveLeft, n):xs) (x,y) = executeCommands xs (x-n,y)
executeCommands ((MoveRight, n):xs) (x,y) = executeCommands xs (x+n,y)
executeCommands ((MoveUp, n):xs) (x,y) = executeCommands xs (x,y+n)
executeCommands ((MoveDown, n):xs) (x,y) = executeCommands xs (x,y-n)

-- Background Material
-- ATMs need to use an algorithm to compute, given an amount, the number of
-- currency notes needed to dispense the money requested by the customer.
-- In this question, you will implement such an algorithm in Haskell.

-- which takes an amount and a list of denominations in ascending order (such as
-- [10, 20, 50, 100]) and returns a list of pairs of denominations and number of
-- notes to be dispensed (with the fewest number of notes).

-- Examples

-- ghci> atmChange 2180 [10, 50, 100, 500]
-- [(500,4),(100,1),(50,1),(10,3)]


-- In the above example, the user has asked for £2180 with allowed denominations of
-- 10, 50, 100, and 500. The algorithm computes that the ATM will need to dispense
-- 4 bills of 500, 1 bill of 100, 1 bill of 50 and 3 bills of 10.
-- Note that 2180 = 4 x 500 + 1 x 100 + 1 x 50 + 3 x 10
-- Some more examples follow.

atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange 0 _ = []
atmChange _ [] = []
atmChange amount (x:xs) = (x, amount `div` x) : atmChange (amount `mod` x) xs

