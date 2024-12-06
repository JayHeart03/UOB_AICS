import Data1
import System.Random



allSmaller :: Ord a => a -> BT a -> Bool
allSmaller x (Leaf y) = x > y
allSmaller x (Fork l y r) = (allSmaller x l) && (allSmaller x r) && (x > y)



allBigger :: Ord a => a -> BT a -> Bool
allBigger x (Leaf y) = x < y
allBigger x (Fork l y r) = (allBigger x l) && (allBigger x r) && (x < y)

isBST :: Ord a => BT a -> Bool
isBST Empty = True
isBST Fork l x r = (allSmaller x l) && (allBigger x r) && (isBST l) && (isBST r)


zipEither :: [a] -> [b] -> Either String [(a,b)]
zipEither [] [] = Right []
zipEither [] (y:ys) = Left "First list is too short"
zipEither (x:xs) [] = Left "Second list is too short"
zipEither (x:xs) (y:ys) = case zipEither xs ys of
                            Left s -> Left s
                            Right zs -> Right ((x,y):zs)

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
                deriving (Eq, Ord, Show, Read, Enum, Bounded)
data WorkingDay = Mon' | Tue' | Wed' | Thu' | Fri'



data Rose a = Branch a [Rose a]

rsize :: Rose a -> Integer
rsize (Branch _ []) = 1
rsize (Branch _ rs) = 1 + sum (map rsize rs)

rheight :: Rose a -> Integer
rheight (Branch _ []) = 1
rheight (Branch _ rs) = 1 + maximum (map rheight rs)


-- Background Material
-- The modules Monad.Control.Except and Monad.Control.State contain
-- definitions of various extensions of the Monad class.  We can use these
-- to write functions which work for any monad m satisfying the correct
-- interface.
-- For example, the MonadError class, defined here,
-- adds a method

-- throwError :: e -> m a


-- allowing you to return an error of type e.
-- Similarly, the MonadState class, defined here adds
-- methods

-- get :: m s
-- put :: s -> m ()
-- modify :: MonadState s m => (s -> s) -> m ()


-- which allow you to manipulate the state carried by the monad m.

-- Implementation Tasks


-- Consider the following type of calculator expressions:

-- data CalcExpr = Val Int
--               | Add CalcExpr CalcExpr
--               | Mult CalcExpr CalcExpr
--               | Div CalcExpr CalcExpr
--               | Sub CalcExpr CalcExpr


-- Write an evaluator which runs in any monad supporting exceptions and which throws an error when it
-- encounters a division by zero.

-- eval :: MonadError String m => CalcExpr -> m Int


-- Notice how we have specialized the error type e from MonadError to String here.  This means that
-- when you encounter a divide by zero, you should return an error message as a string.


-- Now let's imagine a calculator with an integer state which allows the user to update this state using
-- commands. Here is a data type describing a list of commands:

-- data CalcCmd = EnterC
--              | StoreC Int CalcCmd
--              | AddC Int CalcCmd
--              | MultC Int CalcCmd
--              | DivC Int CalcCmd
--              | SubC Int CalcCmd


-- Write a function

-- run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()


-- which runs the given sequence of commands in any monad supporting state and exceptions.
-- Each of the AddC, MultC, DivC and SubC commands should apply
-- the corresponding operation on the provided argument and whatever
-- the current state is.  The StoreC command manually updates the
-- state. Finally, EnterC terminates the calculation, returning the
-- unit type.



-- Examples


-- Here are two calculator expressions:

-- expr1 = Mult (Add (Val 4) (Val 7)) (Div (Val 10) (Val 2))
-- expr2 = Sub (Val 10) (Div (Val 14) (Val 0))


-- The Either type implements the required monadic interface.  Hence we can evaluate using
-- this type as follows:

-- ghci> eval expr1 :: Either String Int
-- Right 55
-- ghci> eval expr2 :: Either String Int
-- Left "Divide by zero!"
-- ghci>




-- Now here are two command sequences:

-- cmd1 = StoreC 7 (AddC 14 (DivC 3 EnterC))
-- cmd2 = StoreC 10 (MultC 2 (DivC 0 EnterC))


-- To run these, we will need to choose an implementation of the state monad to use.  We can do this
-- by introducting the following type synonym:

-- type CS a = StateT Int (Either String) a


-- Now we can do:

-- ghci> runStateT (run cmd1 :: CS ()) (0 :: Int)
-- Right ((),7)
-- ghci> runStateT (run cmd2 :: CS ()) (0 :: Int)
-- Left "Divide by zero!"


-- The value 7 in Right ((),7) is showing us the resulting state of the calculator after the
-- sequence of commands.  This makes sense: we first store 7, then add 14 to the stored value
-- and then divide by 3, leaving a result of 7.

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC = do
    return ()
run (StoreC x c) = do
    put x
    run c
run (AddC x c) = do
    modify ((+)x)
    run c
run (MultC x c) = do
    modify ((*)x)
    run c
run (DivC x c) = do
   if x == 0
       then throwError "Divide by zero!"
       else do
           modify (`div` x)
           run c
run (SubC x c) = do
    modify (\y -> y - x)
    run c

subtract :: Int -> Int -> Int
subtract x y = x - y

-- The drawing below gives an idea of how to cut a given "true" rectangle into squares ("true" rectangle meaning that the two dimensions are different).

-- alternative text

-- Can you translate this drawing into an algorithm?

-- You will be given two dimensions

-- a positive integer length
-- a positive integer width
-- You will return a collection or a string (depending on the language; Shell bash, PowerShell, Pascal and Fortran return a string) with the size of each of the squares.

-- Examples in general form:
-- (depending on the language)

--   sqInRect(5, 3) should return [3, 2, 1, 1]
--   sqInRect(3, 5) should return [3, 2, 1, 1]
--   squaresInRect  5  5 `shouldBe` Nothing
--       squaresInRect  5  3 `shouldBe` Just [3,2,1,1]
--       squaresInRect  3  5 `shouldBe` Just [3,2,1,1]
--       squaresInRect 20 14 `shouldBe` Just [14, 6, 6, 2, 2, 2]

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect l w | l == w = Nothing
                  | l < w = squaresInRect w l
                  | otherwise = Just (squaresInRect' l w)
    where squaresInRect' l w | l == w = [l]
                             | otherwise = w : squaresInRect' (l-w) w


