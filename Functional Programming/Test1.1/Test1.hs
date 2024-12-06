-- Background Material
-- A string is said to be periodic of period n if it repeats after n characters.  For example,
-- the string


-- "abcabcabc" - This string is periodic with period 3, but not periodic with period 2.

-- "ababa" - This string is periodic with period 2.

-- "aaaaaa" - This string is periodic with periods 1,2,3,4,5 and 6.

-- "abcd" - This string is periodic with period 4.

-- Formally, we say that s is periodic if s !! i == s !! i + n for all 0 <= i < length s and n such that i + n < length s

-- Implementation Task
-- Write a function

-- checkPeriodic :: String -> Int -> Bool
-- checkPeriodic = undefined


-- to check whether the given string is periodic with the given period.

-- Examples

-- ghci> checkPeriodic "abcabcabc" 3
-- True
-- ghci> checkPeriodic "aaaaaa" 4
-- True
-- ghci> checkPeriodic "abca" 3
-- True
-- ghci> checkPeriodic "abcd" 3
-- False

checkPeriodic :: String -> Int -> Bool
checkPeriodic s n = all (== True) $ zipWith (==) s (drop n s)


-- which replaces each integer in the list by the boolean which states whether that
-- number is divisible by its index in the list, with indices starting at 1 to avoid dividing by 0.

-- Examples

-- ghci> divisibleByIndex [3,13,9,10,25]
-- [True,False,True,False,True]

-- 3 is divisible by 1


-- 13 is not divisible by 2


-- 9 is divisible by 3

-- and so on
-- ghci> divisibleByIndex [3,13,9,10,25]
-- [True,False,True,False,True]


divisibleByIndex :: [Int] -> [Bool]
divisibleByIndex xs = zipWith (\x y -> x `mod` y == 0) xs [1..] 


-- Implementation Task
-- Write a function

-- findCubes :: Int -> [(Int,Int,Int)]
-- findCubes = undefined


-- which, given a number n, finds all triples (a,b,c) of integers with 1 <= a <= b <= c such that

--   a^3 + b^3 + c^3 = n

-- Examples

-- ghci> take 10 (filter (\n -> length (findCubes n) > 0) [1..])
-- [3,10,17,24,29,36,43,55,62,66]
-- ghci> findCubes 24
-- [(2,2,2)]
-- ghci> findCubes 55
-- [(1,3,3)]

findCubes :: Int -> [(Int,Int,Int)]
findCubes n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a^3 + b^3 + c^3 == n]


-- Background Material
-- Consider the following commands for editing strings:

-- data EditCommand = MoveLeft | MoveRight | Insert Char | BackSpace


-- We will be implementing these commands on a "string with a cursor".  We can represent
-- this situation with a pair of strings:

-- type Text = (String, String)


-- The first string consists of the characters to the left of the cursor, stored backwards for convenience, and
-- the right string consists of the characters after the cursor.
-- For example, suppose we have the string "Heelo everybody!", and we would like to fix the spelling mistake.  Here is what the process would look like:



-- If a command is not possible, the text should be left unchanged, with no error given.

-- Implementation Task
-- Implement functions which update the current Text, both for single commands, as well as for a list of them, returning the final state.


-- Examples

-- ghci> edits (replicate 6 MoveRight) ("", "This is a String")
-- ("i sihT","s a String")
-- ghci> edits (map Insert "Hello") ("", " World!")
-- ("olleH"," World!")
-- ghci> edit MoveLeft ("","Start of Line")
-- ("","Start of Line")
-- ghci> edits (replicate 9 BackSpace) (" ,eybdooG", "World!")
-- ("","World!")

data EditCommand = MoveLeft | MoveRight | Insert Char | BackSpace

type Text = (String, String)

cmds = [ MoveRight
       , MoveRight
       , BackSpace
       , MoveRight
       , Insert 'l'
       , MoveLeft
       , MoveLeft
       , MoveLeft
       ]

cmds2 = [ MoveRight
        , MoveRight
        , Insert 'x'
        , Insert 'y'
        , Insert 'z'
        , MoveLeft
        , BackSpace
        ] 

edit :: EditCommand -> Text -> Text
edit _ (a, []) = (a, [])
edit _ ([], b) = ([] ,b)
edit MoveLeft (a:as, b) = (as, a:b)
edit MoveRight (a, b:bs) = (b:a, bs)
edit (Insert c) (a, b) = (c:a, b)
edit BackSpace (a:as, b) = (as, b)

edits :: [EditCommand] -> Text -> Text
edits [] t = t


-- Background Material
-- We will think of a function f of type f :: [Bool] -> Bool as a "boolean function of multiple variables".  We say this function is solvable in dimension n if there is a list xs :: [Bool] of length n for which the value f xs is True.

-- Implementation Task
-- Write a function solvable which, given a function f :: [Bool] -> Bool and a non-negative integer n, decides whether or f is solvable in dimension n.

-- Examples
-- Consider the function f defined by

-- f (a : b : c : _) = a || b || c


-- Then we have

-- solvable f 3 = True


-- Now consider the function

-- g (a:_) = a && not a


-- then
-- solvable g 3 = False


solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable f n = any f $ map (take n . (++ repeat False)) $ replicateM n [True, False]








checkPeriodic "abcd" 1
checkPeriodic "abcd" 2
checkPeriodic "abcd" 3
checkPeriodic "abcd" 4
checkPeriodic "abcabca" 3
checkPeriodic "abcabc" 3

divisibleByIndex [3,13,9,10,25]

take 10 (filter (\n -> length (findCubes n) > 0) [1..])

edits (replicate 6 MoveRight) ("", "This is a String")
edits (map Insert "Hello") ("", " World!")
edits (replicate 9 BackSpace) (" ,eybdooG", "World!")
edit MoveLeft ("","Start of Line")
edit MoveRight ("Start of Line","")
edit BackSpace ("","Start of Line")

solvable g 3
solvable f 3