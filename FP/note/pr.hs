-- DESCRIPTION:
-- The Western Suburbs Croquet Club has two categories of membership, Senior and Open. They would like your help with an application form that will tell prospective members which category they will be placed.

-- To be a senior, a member must be at least 55 years old and have a handicap greater than 7. In this croquet club, handicaps range from -2 to +26; the better the player the lower the handicap.

-- Input
-- Input will consist of a list of pairs. Each pair contains information for a single potential member. Information consists of an integer for the person's age and an integer for the person's handicap.

-- Output
-- Output will consist of a list of string values (in Haskell and C: Open or Senior) stating whether the respective member is to be placed in the senior or open category.

-- Example
-- input =  [[18, 20], [45, 2], [61, 12], [37, 6], [21, 21], [78, 9]]
-- output = ["Open", "Open", "Senior", "Open", "Open", "Senior"]

data Membership = Open | Senior deriving (Eq, Show)
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior [] = []
openOrSenior ((age, handicap):xs) = if age >= 55 && handicap > 7 then Senior : openOrSenior xs else Open : openOrSenior xs

-- ROT13 is a simple letter substitution cipher that replaces a letter with the letter 13 letters after it in the alphabet. ROT13 is an example of the Caesar cipher.

-- Create a function that takes a string and returns the string ciphered with Rot13. If there are numbers or special characters included in the string, they should be returned as they are. Only letters from the latin/english alphabet should be shifted, like in the original Rot13 "implementation".

rot13 :: String -> String
rot13 [] = []
rot13 (x:xs) = if x `elem` ['a'..'z'] then rot13' x : rot13 xs else if x `elem` ['A'..'Z'] then rot13' x : rot13 xs else x : rot13 xs

rot13' :: Char -> Char
rot13' x = if x `elem` ['a'..'m'] || x `elem` ['A'..'M'] then chr (ord x + 13) else chr (ord x - 13)

-- Some numbers have funny properties. For example:

-- 89 --> 8¹ + 9² = 89 * 1

-- 695 --> 6² + 9³ + 5⁴= 1390 = 695 * 2

-- 46288 --> 4³ + 6⁴+ 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51

-- Given a positive integer n written as abcd... (a, b, c, d... being digits) and a positive integer p

-- we want to find a positive integer k, if it exists, such that the sum of the digits of n taken to the successive powers of p is equal to k * n.
-- In other words:

-- Is there an integer k such as : (a ^ p + b ^ (p+1) + c ^(p+2) + d ^ (p+3) + ...) = n * k

-- If it is the case we will return k, if not return -1.

-- Note: n and p will always be given as strictly positive integers.

-- digpow 89 1 should return 1 since 8¹ + 9² = 89 = 89 * 1
-- digpow 92 1 should return -1 since there is no k such as 9¹ + 2² equals 92 * k
-- digpow 695 2 should return 2 since 6² + 9³ + 5⁴= 1390 = 695 * 2
-- digpow 46288 3 should return 51 since 4³ + 6⁴+ 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51


digpow :: Integer -> Integer -> Integer
digpow n p = if sum == n * k then k else -1
  where
    sum = sumDigits n p
    k = sum `div` n

sumDigits :: Integer -> Integer -> Integer
sumDigits n p = sum $ zipWith (^) (digits n) [p..]

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

-- The number 89 is the first integer with more than one digit that fulfills the property partially introduced in the title of this kata. What's the use of saying "Eureka"? Because this sum gives the same number.

-- In effect: 89 = 8^1 + 9^2

-- The next number in having this property is 135.

-- See this property again: 135 = 1^1 + 3^2 + 5^3

-- We need a function to collect these numbers, that may receive two integers a, b that defines the range [a, b] (inclusive) and outputs a list of the sorted numbers in the range that fulfills the property described above.

-- Let's see some cases (input -> output):

-- 1, 10 -> [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- 1, 100 -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 89]
-- If there are no numbers of this kind in the range [a, b] the function should output an empty list.

-- 90, 100 --> []

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter (\x -> x == sumDigits x 1) [a..b]

sumDigits :: Int -> Int -> Int
sumDigits n p = sum $ zipWith (^) (digits n) [p..]

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]


-- Introduction
-- There is a war and nobody knows - the alphabet war!
-- There are two groups of hostile letters. The tension between left side letters and right side letters was too high and the war began.

-- Task
-- Write a function that accepts fight string consists of only small letters and return who wins the fight. When the left side wins return Left side wins!, when the right side wins return Right side wins!, in other case return Let's fight again!.

-- The left side letters and their power:

--  w - 4
--  p - 3
--  b - 2
--  s - 1
-- The right side letters and their power:

--  m - 4
--  q - 3
--  d - 2
--  z - 1
-- The other letters don't have power and are only victims.

-- Example
-- AlphabetWar("z");        //=> Right side wins!
-- AlphabetWar("zdqmwpbs"); //=> Let's fight again!
-- AlphabetWar("zzzzs");    //=> Right side wins!
-- AlphabetWar("wwwwwwz");  //=> Left side wins!

alphabetWar :: String -> String
alphabetWar x = if left > right then "Left side wins!" else if right > left then "Right side wins!" else "Let's fight again!"
  where
    left = sum $ map (\x -> if x == 'w' then 4 else if x == 'p' then 3 else if x == 'b' then 2 else if x == 's' then 1 else 0) x
    right = sum $ map (\x -> if x == 'm' then 4 else if x == 'q' then 3 else if x == 'd' then 2 else if x == 'z' then 1 else 0) x


-- If you have completed the Tribonacci sequence kata, you would know by now that mister Fibonacci has at least a bigger brother. If not, give it a quick look to get how things work.

-- Well, time to expand the family a little more: think of a Quadribonacci starting with a signature of 4 elements and each following element is the sum of the 4 previous, a Pentabonacci (well Cinquebonacci would probably sound a bit more italian, but it would also sound really awful) with a signature of 5 elements and each following element is the sum of the 5 previous, and so on.

-- Well, guess what? You have to build a Xbonacci function that takes a signature of X elements - and remember each next element is the sum of the last X elements - and returns the first n elements of the so seeded sequence.

    -- xbonacci [0,1] 10 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
    -- xbonacci [1,1] 10 `shouldBe` [1,1,2,3,5,8,13,21,34,55]
    -- xbonacci [0,0,0,0,1] 10 `shouldBe` [0,0,0,0,1,1,2,4,8,16]
    -- xbonacci [1,0,0,0,0,0,1] 10 `shouldBe` [1,0,0,0,0,0,1,2,3,6]

im
xbonacci :: Num a => [a] -> Int -> [a]
xbonacci _ 0 = []
xbonacci xs n = haed xs : xbonacci (tail xs ++ [sum xs]) (n - 1)

-- Question 7
-- Simple Encryption #1 - Alternating Split
-- Implement a pseudo-encryption algorithm which given a string S and an integer N concatenates all the odd-indexed characters of S with all the even-indexed characters of S, this process should be repeated N times.

-- Examples:
-- encrypt("012345", 1)  =>  "135024"
-- encrypt("012345", 2)  =>  "135024"  ->  "304152"
-- encrypt("012345", 3)  =>  "135024"  ->  "304152"  ->  "012345"

-- encrypt("01234", 1)  =>  "13024"
-- encrypt("01234", 2)  =>  "13024"  ->  "32104"
-- encrypt("01234", 3)  =>  "13024"  ->  "32104"  ->  "20314"
-- Together with the encryption function, you should also implement a decryption function which reverses the process.

-- If the string S is an empty value or the integer N is not positive, return the first argument without changes.
import Data.List
encrypt :: String -> Int -> String
encrypt s n | n <= 0 = s
            | otherwise = encrypt (concat $ transpose [even, odd]) (n - 1)
  where
    (odd, even) = splitAt (length s `div` 2) s

decrypt :: String -> Int -> String
decrypt s n | n <= 0 = s
            | otherwise = decrypt (concat $ transpose [even, odd]) (n - 1)
  where
    (odd, even) = splitAt (length s `div` 2) s

-- Alphabet symmetry
-- Consider the word "abode". We can see that the letter a is in position 1 and b is in position 2. In the alphabet, a and b are also in positions 1 and 2. Notice also that d and e in abode occupy the positions they would occupy in the alphabet, which are positions 4 and 5.

-- Given an array of words, return an array of the number of letters that occupy their positions in the alphabet for each word. For example,

-- solve(["abode","ABc","xyzD"]) = [4, 3, 1]

-- solve ["abode","ABc","xyzD"] `shouldBe` [4,3,1]
-- solve ["abide","ABc","xyz"] `shouldBe` [4,3,0]
-- solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] `shouldBe` [6,5,7]
-- See test cases for more examples.

-- Input will consist of alphabet characters, both uppercase and lowercase. No spaces.

solve :: [String] -> [Int]
solve [] = []
solve = map (\x -> length $ filter (\(a, b) -> a == b) $ zip x ['a'..'z'])

