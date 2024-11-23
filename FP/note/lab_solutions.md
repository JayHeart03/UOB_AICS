# Problem Sheet for Week 2

## Lab Video

We will begin with a short introductory [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ea24b3c5-c0ed-4544-ae13-af2300d9503f)

## Ill-typed Expressions

1. Write five ill-typed expressions in Haskell. 

	1. `tail 3`
	1. `5 == "hello"`
	1. `'b' + 4`
	1. `toUpper 7`
	1. `if True then 6 else "seven"`
	
## Polymorphism


1. Find out the types of the following functions. Decide if they are polymorphic.

	  | Function    | Type                | Polymorphic |
	  |-------------|---------------------|-------------|
	  | `fst`       | `(a, b) -> a`       | yes         |
	  | `(++)`      | `[a] -> [a] -> [a]` | yes         |
	  | `not`       | `Bool -> Bool`      | no          |
	  | `head`      | `[a] -> a`          | yes         |
	  | `tail`      | `[a] -> [a]`        | yes         |
	  | `id`        | `a -> a`            | yes         |

2. Explain, in your own words, what the function `zip` does. In the expression `zip ['x', 'y'] [False]`, what are the type variables `a` and `b` of `zip :: [a] -> [b] -> [(a, b)]` instantiated by?

	The function zip traverses the two lists simultaneously while
	returning the pairs of the elements it encounters in a new list.
	If the lists are not the same length, the result will have the
	length of the shorter list.  In the example, we have `a = Char`
	and `b = Bool`.
	
3. Find a polymorphic function in the GHC [standard library](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html) whose type contains 3 type variables or more.

	Examples are `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]` and `flip :: (a -> b -> c) -> b -> a -> c`.
	
4. Read Section 3.7 of Programming in Haskell. Compare the types of the examples given there with the types `ghci` indicates. (Note: some of the types that `ghci` shows use "type classes" - you will learn about these in the next lesson.)

	The only one which differs is `length :: Foldable t => t a -> Int`
	which is generalized from just the type `[a]` of lists to
	arbitrary members of the `Foldable` typeclass.

## Standard Library Functions and Hoogle

Look up the following functions for manipulating lists on
[Hoogle](https://hoogle.haskell.org/) and write down their types.  For
each function, read the description and try the function on a sample list.

 | Function  | Type                                     |
 |-----------|------------------------------------------|
 | length    | Foldable t => t a -> Int                 |
 | reverse   | [a] -> [a]                               |
 | tail      | HasCallStack => [a] -> [a]               |
 | head      | HasCallStack => [a] -> a                 |
 | take      | Int -> [a] -> [a]                        |
 | drop      | Int -> [a] -> [a]                        |
 | takeWhile | (a -> Bool) -> [a] -> [a]                |
 | dropWhile | (a -> Bool) -> [a] -> [a]                |
 | filter    | (a -> Bool) -> [a] -> [a]                |
 | all       | Foldable t => (a -> Bool) -> t a -> Bool |
 | any       | Foldable t => (a -> Bool) -> t a -> Bool |
 | map       | (a -> b) -> [a] -> [b]                   |

## Functions in Haskell

Here are some example functions which you can try to implement to get started using both the concepts from the lecture as well as the library functions you found above.

1. Write a function `orB :: Bool -> Bool -> Bool` that returns `True` if at least one argument is `True`.

		orB :: Bool -> Bool -> Bool
		orB True True = True
		orB True False = True
		orB False True = True 
		orB False False = False

		orB' :: Bool -> Bool -> Bool
		orB' True _ = True
		orB' _ True = True
		orB' _ _ = False
		
1. Write a function `swap :: (a, b) -> (b, a)` that swaps the elements of a pair.

		swap :: (a,b) -> (b,a)
		swap (x,y) = (y,x)
		
1. Write a function that removes both the first and the last element of a list.

		removeFirstAndLast :: [a] -> [a]
		removeFirstAndLast = reverse . tail . reverse . tail 
		
1. Write a function which returns the reverse of a list if its length is greater than 7.  Now modify the function so that the cutoff length is a parameter.

		reverse7 :: [a] -> [a]
		reverse7 l = if (length l > 7) then reverse l else l 

		reverse7guard :: [a] -> [a]
		reverse7guard l | length l > 7 = reverse l 
                        | otherwise    = l 
                
        reverseParam :: Int -> [a] -> [a]
		reverseParam n l | length l > n = reverse l
                         | otherwise    = l 
						 
1. Write a function which doubles all the elements of a list `l :: [Int]` and then keeps only those which are greater than 10. 

        doubleGtTen :: [Int] -> [Int]
        doubleGtTen l = filter (> 10) $ map (\x -> x + x) l 
		
1. Write a function to return the reverse of a string with all its alphabetic elements capitalized. (The function `toUpper :: Char -> Char` in the `Data.Char` library may be useful here.)

		revToUpper :: String -> String 
		revToUpper s = reverse $ map toUpper s

# Homework for Week 2

This homework will not be assessed.  You must complete it, however, in order to prepare for the tests.

## Writing More Functions

1. Write a function to pair each element of a list with its index.

		withIndex :: [a] -> [(Int,a)]
		withIndex l = zip [0 .. length l - 1] l 

		withIndex' :: [a] -> [(Int,a)]
		withIndex' l = [ (i, l !! i) | i <- [0 .. length l-1] ]
		
1. Using guarded equations, write a function of type `Int -> Int -> Bool` that returns `True` if the first argument is greater than the second and less than twice the second.

		betweenNand2n :: Int -> Int -> Bool
		betweenNand2n k n | k > n && k < 2 * n = True
                          | otherwise          = False 
						  
1. (Adapted and expanded from the book "Programming in Haskell)
   Define three variants of a function `third :: [a] -> a` that returns the third element in any list that contains at least this many elements, using
   
    1. `head` and `tail`
	    ```haskell
		thirdHeadTail :: [a] -> a
		thirdHeadTail = head . tail . tail 
		```
		
    2. list indexing `!!`
	
		```haskell
		thirdIndex :: [a] -> a
		thirdIndex l = l !! 2 
		``` 
	
    3. pattern matching
	
		```haskell
		thirdMatching :: [a] -> a
		thirdMatching (_:_:x:_) = x 
		```

1. (Adapted and expanded from the book "Programming in Haskell)
   Define a function `safetail :: [a] -> [a]` that behaves like tail except that it maps `[]` to `[]` (instead of throwing an error). Using `tail` and `isEmpty :: [a] -> Bool`,
   define `safetail` using
   
   1. a conditional expression
   
	   ```haskell
	   isEmpty :: [a] -> Bool
	   isEmpty [] = True
	   isEmpty _  = False
	   
	   safeTailCond :: [a] -> [a] 
	   safeTailCond l = if isEmpty l then [] else tail l 
	   ```
   2. guarded equations
   
	   ```haskell
	   safeTailGuard :: [a] -> [a]
	   sageTailGuard l | isEmpty l = []
	                   | otherwise = tail l 
	   ```
   
   3. pattern matching
   
	   ```haskell
	   safeTailMatch :: [a] -> [a]
	   safeTailMatch [] = []
	   safeTailMatch (_:xs) = xs
	   ```

## Type Classes and Instances 

1. Run, and understand, the following examples:

	| Expression                          | Result  | Explanation                                                           |
	|-------------------------------------|---------|-----------------------------------------------------------------------|
	| `False == 'c'`                      | Error   | The left side has type `Bool` while the right has type `Char`         |
	| `False == True`                     | `False` |                                                                       |
	| `False == not`                      | Error   | The left side had type `Bool` while the right has type `Bool -> Bool` |
	| `False == not True`                 | `True`  |                                                                       |
	| `not == id`                         | Error   | No instance of `Eq` for `Bool -> Bool`                                |
	| `[not] == [ (id :: Bool -> Bool) ]` | Error   | No instance of `Eq` for `Bool -> Bool`                                |
	
1. Find all the basic instances of the type class `Bounded` that are defined in the GHC Prelude (the libraries that are loaded when starting `ghci`, without importing any additional libraries). Find out what `minBound` and `maxBound` are for each of the instances.

    | Type       | minBound               | maxBound               |
    |------------|------------------------|------------------------|
    | `Word`     | `0`                    | `18446744073709551615` |
    | `Ordering` | `LT`                   | `GT`                   |
    | `Int`      | `-9223372036854775808` | `9223372036854775807`  |
    | `Char`     | `\NUL`                 | `'\1114111'`           |
    | `Bool`     | `False`                | `True`                 |

2. What type classes do the type classes `Fractional`, `Floating`, `Integral` extend? What functions do they provide? Which type class would you choose to implement a trigonometric calculus?

    | Typeclass    | Extends            | Provides                        |
    |--------------|--------------------|---------------------------------|
    | `Fractional` | `Num`              | `(/) :: a -> a -> a`            |
    |              |                    | `recip :: a -> a`               |
    |              |                    | `fromRational :: Rational -> a` |
    | `Floating`   | `Fractional`       | `pi :: a`                       |
    |              |                    | `exp :: a -> a`                 |
    |              |                    | `log :: a -> a`                 |
    |              |                    | ...                             |
    | `Integral`   | `Real a`, `Enum a` | `quot :: a -> a -> a`           |
    |              |                    | `rem :: a -> a -> a`            |
    |              |                    | `div :: a -> a -> a`            |
    |              |                    | ...                             |



# Problem Sheet for Week 3

## Lab Video

We will begin with a short introductory [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a0391430-ddbf-4832-848f-af2a01288477)

## Recursive Functions

1. Without looking at the standard prelude, define the following library functions using recursion:

	* Decide if all logical values in a list are true:

		```haskell
		and' :: [Bool] -> Bool
        and' [] = True
        and' (x:xs) = x && and' xs
		```
	* Concatenate a list of lists:

		```haskell
		concat' :: [[a]] -> [a]
        concat' [] = []
        concat' (x:xs) = x ++ concat' xs
		```
	* Produce a list with n identical elements:

		```haskell
		replicate' :: Int -> a -> [a]
        replicate' 0 _ = []
        replicate' n x = [x] ++ replicate' (n-1) x
		```

	* Select the nth element of a list:

		```haskell
        (!!!) :: [a] -> Int -> a
        (!!!) [] _ = undefined
        (!!!) (x:xs) n | n == 0 = x
               | otherwise = (!!!) xs (n-1)
		```

	* Decide if a value is an element of a list:

		```haskell
        elem' :: Eq a => a -> [a] -> Bool
        elem' y [] = False
        elem' y (x:xs) | x == y = True
               | otherwise = elem' y xs
        ```

1. Define a recursive function

	```haskell
	merge :: Ord a => [a] -> [a] -> [a]
    merge [] y = y
    merge x [] = x
    merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                        | otherwise = y:(merge (x:xs) ys)
	```
	that merges two sorted lists of values to give a single sorted list.

	For example:

	```hs
	> merge [2,5,6] [1,3,4]
	[1,2,3,4,5,6]
	```

## List Comprehensions

Please read the following handout before proceeding further [List Comprehensions](/files/LectureNotes/Sections/list_comprehensions.md)

1. A triple (x,y,z) of positive integers is called pythagorean if
x^2 + y^2 = z^2 . Using a list comprehension, define a function:

    ```haskell
    pyths :: Int -> [(Int,Int,Int)]
    pyths n = [ (a,b,c) | a <- ns , b <- ns , c <- ns , a^2 + b^2 == c^2 ]
      where ns = [1..n]
    ```

    that maps an integer n to all such triples with components in
    [1..n]. For example:

    ```hs
    > pyths 5
    [(3,4,5),(4,3,5)]
    ```

1. A positive integer is perfect if it equals the sum of all of its
   factors, excluding the number itself. Using a list comprehension,
   define a function

    ```haskell
    divisors :: Int -> [Int]
    divisors n = [ f | f <- [1 .. n-1] , n `mod` f == 0 ]

    perfects :: Int -> [Int]
    perfects n = [ p | p <- [1..n] , sum (divisors p) == p ]
    ```

    that returns the list of all perfect numbers up to a given limit. For example:

    ```hs
    > perfects 500
    [6,28,496]
    ```

	Many variations of this exercise are possible:

    * A number which is less than the sum of its proper divisors is called [abundant](https://en.wikipedia.org/wiki/Abundant_number).
    ```haskell
    abundants :: Int -> [Int]
    abundants n = [ a | a <- [1..n] , sum (divisors a) > a ]
    ```

    * A number which is greater than the sum of its proper divisions is called [deficient](https://en.wikipedia.org/wiki/Deficient_number).
    ```haskell
    deficients :: Int -> [Int]
    deficients n = [ a | a <- [1..n] , sum (divisors a) < a ]
    ```

    * A number for which the sum of all its divisors (including itself) is greater than
	the sum of the divisors of any smaller number is called [highly abundant](https://en.wikipedia.org/wiki/Highly_abundant_number).
    ```haskell
    highlyAbundant :: Int -> [Int]
    highlyAbundant n = [ a | a <- [1..n] ,
                   and [ sum (a:divisors a) > sum (b:divisors b) | b <- [1..a-1] ] ]
    ```

	For each of these variations, write a function which finds all the numbers with the
	stated property below a given number.

1. The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers:

    ![dot product](./assets/dot-prod.png)

    Using a list comprehension, define a function that returns the scalar product of two lists.

    ```haskell
    dotProd :: [Int] -> [Int] -> Int
    dotProd x y = sum [ i * j | (i,j) <- zip x y ]
    ```

1.  **Harder** Implement [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication) where matrices are represented by lists of lists of integers.  One possibility, for example, would be to take the dimensions of the matrices as arguments, so that your function would have type:

    ```haskell
      matrix_mul :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
      matrix_mul n p m x y =
          [ [ sum [ ((x !! i) !! k) * ((y !! k) !! j)
              | k <- [0..p-1] ]
              | j <- [0..m-1] ]
              | i <- [0..n-1] ]
	```

# Homework for Week 3

This homework will be marked but not counted towards your final grade. You will write your code on JupyterLab and
submit via Canvas (link below).

- Create new directory in your workspace on JupyterLab and upload/copy the files found under the `Homework3` directory.
- Copy the file `Homework3-Template.hs` to a new file called `Homework3.hs`.
- Solve the exercises below in the file `Homework3.hs`.
- Run the pre-submit script to check for any (compilation) errors **before** submitting by running in the terminal:

```bash
$ ./presubmit.sh Homework3
```
- Submit your file `Homework3.hs` via Canvas at https://canvas.bham.ac.uk/courses/65655/assignments/384995 .
- You also require the file `Types.hs` to be in the same directory as `Homework3.hs`. The file `Types.hs` should **not** be modified, and should **not** be submitted on Canvas.

0. Copy the file `Homework3-Template.hs` to a new file `Homework3.hs`.

1. We classify cars into categories according to their gas usage, measured in liters per 100 kilometers.
Consider
```haskell
data Classification = Low | Medium | High | SuperHigh deriving (Show)
```
(The `deriving (Show)` is there so that Haskell can print classifications when you're testing your program in the terminal. We will discuss this in more detail later.)

Write a function
```hs
gasUsage :: (Fractional a, Ord a) => a -> Classification
```
according to the table

| Gas Usage g        | g < 3 | 3 <= g < 5 |  5 <= g < 7 | 7 <= g    |
|--------------------|-------|------------|-------------|-----------|
| Classification     | Low   | Medium     | High        | SuperHigh |


```haskell
gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage g | g < 3           = Low
           | 3 <= g && g < 5 = Medium
           | 5 <= g && g < 7 = High
           | otherwise       = SuperHigh
```

2. (From "Programming in Haskell", Section 4.8 "Exercises", Exercise 8)

The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:
- consider each digit as a separate number;
- moving left, double every other number from the second last;
- subtract 9 from each number that is now greater than 9;
- add all the resulting numbers together;
- if the total is divisible by 10, the card number is valid.

See also the [Wikipedia entry on the Luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm).

Define a function
```haskell
luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9 else 2 * x
```
that doubles a digit and
subtracts 9 if the result is greater than 9. For example:
```hs
> luhnDouble 3
6
> luhnDouble 6
3
```
Using `luhnDouble` and the integer remainder function `mod`, define a function
```haskell
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (d + (luhnDouble c) + b + (luhnDouble a)) `mod` 10 == 0
```
that decides if a four digit bank card number is valid (according to the Luhn algorithm described above).
For example:
```hs
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False
```

3. Run the pre-submit script to check for any (compilation) errors **before** submitting by running in the terminal:
```bash
$ ./presubmit.sh Homework3
```



# Problem Sheet for Week 4

## Lab Video

We will begin with a short introductory [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=81a4b59a-cc63-4aad-b48b-af3101318f99)

## List comprehensions and higher-order functions

1. Express the comprehension `[f x | x <- xs, p x]` using the functions `map` and `filter`. The function type is given as:
	```haskell
    fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
    fun f p xs = map f (filter p xs)
	```
	For example:
	```
	> fun (^2) even [1..20]
	[4,16,36,64,100,144,196,256,324,400]

	> fun (^2) odd [1..20]
	[1,9,25,49,81,121,169,225,289,361]
	```
1. Redefine `map f` and `filter p` using `foldr` and `foldl`. For your reference, here are the definitions of `map` and `filter` from lecture notes. HINT. Read about the `foldr` and `foldl` functions in the handout [higher-order functions](/files/LectureNotes/Sections/higher-order_functions.md) and Chapter 7.3 and 7.4 of [Programming in Haskell](https://bham.rl.talis.com/link?url=https%3A%2F%2Fapp.kortext.com%2FShibboleth.sso%2FLogin%3FentityID%3Dhttps%253A%252F%252Fidp.bham.ac.uk%252Fshibboleth%26target%3Dhttps%253A%252F%252Fapp.kortext.com%252Fborrow%252F382335&sig=70da9a4ff905dba3523840088f10e61e90877af4795f3070b3775767fa856348).
	```hs
	map :: (a -> b) -> [a] -> [b]
	map f []     = []
	map f (x:xs) = f x : map f xs

	filter :: (a -> Bool) -> [a] -> [a]
	filter p [] = []
	filter p (x:xs)
	   | p x       = x : filter p xs
	   | otherwise = filter p xs

	```

	```haskell
    map' :: (a -> b) -> [a] -> [b]
    map' f xs = foldr (\y ys -> (f y):ys) [] xs

    map'' :: (a -> b) -> [a] -> [b]
    map'' f xs = foldl (\ys y -> ys ++ [(f y)]) [] xs


    filter' :: (a -> Bool) -> [a] -> [a]
    filter' p xs = foldr (\y ys -> if p y then y:ys else ys) [] xs

    filter'' :: (a -> Bool) -> [a] -> [a]
    filter'' p xs = foldl (\ys y -> if p y then ys . (y:) else ys) id xs []
	```

1. Define a function `altMap :: (a -> b) -> (a -> b) -> [a] -> [b]` that alternatively applies the two argument functions to successive elements in a list.

	For example:
	```hs
	> altMap (+10) (+100) [0,1,2,3,4]
	[10,101,12,103,14]
	```

	```haskell
    altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
    altMap f g [] = []
    altMap f g (x:[]) = f x : []
    altMap f g (x:y:xs) = (f x):(g y):(altMap f g xs)
	```

1. **Harder** Church Numerals

	It is possible to represent the natural numbers (i.e. 0,1,2,...) using higher
	order functions of type

	```hs
	(a -> a) -> (a -> a)
	```

	These are called **Church Numerals**. The encoding works like
	this: the input to a function of the above type is an element `f`
	of type `a -> a`.  Since such a function takes input values of
	type `a` and also produces output values of type `a`, this means
	it can be *iterated*.  So we will represent the numeral `n` by a
	the element of type `(a -> a) -> (a -> a)` which iterates its
	argument n times.

	To see the idea, the first few examples of numerals are written like
    this:

	```hs
	zero :: (a -> a) -> (a -> a)
	zero f x = x

	one :: (a -> a) -> (a -> a)
	one f x = f x

	two :: (a -> a) -> (a -> a)
	two f x = f (f x)

    three :: (a -> a) -> (a -> a)
	three f x = f (f (f x))
	```

	* Write a function to implement *addition* of Church numerals

	```haskell
    addChurch :: ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a))
    addChurch m n f x = m f (n f x)
	```

    * Write a function to implement *multiplication* of Church numerals

	```haskell
    mulChurch :: ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a))
    mulChurch m n f = m (n f)
	```

## Defining the prelude `concat` function in four different ways

The prelude function `concat` concatenates a list of lists, getting a single list. You will define it in four different ways, and test your implementations for correctness and efficiency.

1. Define it with comprehensions and no recursion. HINT: You will need two generators, one to extract a list xs from the list of lists xss, and another to extract an element x from the list xs, and put this in the result of the comprehension.

	```haskell
    concat1 :: [[a]] -> [a]
    concat1 xss = [x | xs <- xss, x <- xs]
	```

1. Define the same function using recursion instead. HINT. Find and use the prelude function `++`.

	```haskell
    concat2 :: [[a]] -> [a]
    concat2 [] = []
    concat2 (xs:xss) = xs ++ concat2 xss
    ```

1. Define the same function using `foldr` and `foldl`, and without recursion or list comprehensions.

	```haskell
    concat3 :: [[a]] -> [a]
    concat3 xss = foldr (++) [] xss

    concat4 :: [[a]] -> [a]
    concat4 xss = foldl (++) [] xss
	```

## Testing your `concat` functions

1. We can test the above functions as follows:

	```hs
        list = [[2,3,4],[5,6,7],[8,9,10]]
        concat1_test = concat1 list == concat list
        concat2_test = concat2 list == concat list
        concat3_test = concat3 list == concat list
        concat4_test = concat4 list == concat list
	```

   Run the above tests under `ghci`. Write also tests for the empty list. Do your functions work with the empty list?

1. Test for speed

	```hs
        biglist = replicate 1000 [1..1000]
        concat1_test2 = concat1 biglist == concat biglist
        concat2_test2 = concat2 biglist == concat biglist
        concat3_test2 = concat3 biglist == concat biglist
        concat4_test2 = concat4 biglist == concat biglist
	```

   Now run `:set +s` at the `ghci` prompt. This asks `ghci` to print time and memory usage statistics.

1. Better run-time test. Check how time increases when n increases in the following tests.

	```hs
        nlist n = replicate n [1..n]
        concat1_test3 n = concat1 (nlist n) == concat (nlist n)
        concat2_test3 n = concat2 (nlist n) == concat (nlist n)
        concat3_test3 n = concat3 (nlist n) == concat (nlist n)
        concat4_test3 n = concat4 (nlist n) == concat (nlist n)
	```

Which implementation(s) are more efficient? Some of them run in linear time and other(s) in quadratic time. Which ones?