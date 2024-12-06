-- Background Material
-- In this exercise, we return to a version of binary trees which stores data only at the nodes.  We will use the following definition:

-- data BinTree a = Empty | Node (BinTree a) a (BinTree a)
--   deriving (Eq, Show)


-- Next, we define a type Route which will describe the "route" one must take to arrive at a particular node in one of our trees.

-- data Direction = GoLeft | GoRight
--   deriving (Eq, Show, Bounded, Enum)

-- type Route = [Direction]


-- So a route is a list of directions, which tell you whether to go left or right,
-- starting from the root of the binary tree. For example,

-- The route to the root of any binary tree is [] :: Route.
-- In the tree Node (Node Empty 'b' Empty) 'a' (Node (Node Empty 'd' Empty) 'c' (Node Empty 'e' Empty)), pictured below, the route [GoLeft]
-- takes you to the node with value 'b', while the routes to the nodes
-- with values 'd' and 'e' are [GoRight,GoLeft] and
-- [GoRight,GoRight] respectively.

--                'a'
--                / \
--               /   \
--             'b'   'c'
--                   / \
--                  /   \
--                'd'   'e'

-- such that updateNodes r f t applies f to the values of all nodes along the route r in the tree t.
-- NB:

-- If you run out of directions before hitting a leaf, e.g. if running
-- updateNodes on the route [GoRight] in the tree above, then you
-- stop and do not modify the remainder of the tree (the values 'd'
-- and 'e' in the example).
-- If the route is too long, e.g. if running updateNodes on the
-- route [GoLeft,GoLeft] in the tree above, then you discard the
-- remainder of the route (so in the example, you would only update the
-- values 'a' and 'b' and then stop).

-- The examples given below should also help to clarify these points.

-- Examples
-- For the following binary tree:

--        1
--       / \
--      2   \
--     / \	  99
--    /   \   \
--   3     4   \
--             100



-- *Main> let t = Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 1 (Node Empty 99 (Node Empty 100 Empty))
-- *Main> updateNodes [] (*8) t
-- Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 99 (Node Empty 100 Empty))
-- *Main> updateNodes [GoRight] (*8) t
-- Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 100 Empty))
-- *Main> updateNodes [GoRight,GoLeft] (*8) t
-- Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 100 Empty))
-- *Main> updateNodes [GoRight,GoRight] (*8) t
-- Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 800 Empty))
-- *Main> updateNodes [GoRight,GoRight,GoLeft] (*8) t
-- Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 800 Empty))
-- *Main> updateNodes [GoLeft,GoLeft,GoLeft] (*15) t
-- Node (Node (Node Empty 45 Empty) 30 (Node Empty 4 Empty)) 15 (Node Empty 99 (Node Empty 100 Empty))

updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes [] _ Empty = Empty
updateNodes [] f (Node l v r) = Node l (f v) r
updateNodes _ _ Empty = Empty
updateNodes (GoLeft:rs) f (Node l v r) = updateNodes( Node (updateNodes rs f l) f(v) r)
updateNodes (GoRight:rs) f (Node l v r) = updateNodes( Node l f(v) (updateNodes rs f r))

    






-- Consider the following type of calculator expressions:

-- data CalcExpr = Val Int
--               | Add CalcExpr CalcExpr
--               | Mult CalcExpr CalcExpr
--               | Div CalcExpr CalcExpr
--               | Sub CalcExpr CalcExpr


-- Write an evaluator which runs in any monad supporting exceptions and which throws an error when it
-- encounters a division by zero.

-- Notice how we have specialized the error type e from MonadError to String here.  This means that
-- when you encounter a divide by zero, you should return an error message as a string.
-- Write an evaluator which runs in any monad supporting exceptions and which throws an error when it
-- encounters a division by zero.

-- expr1 = Mult (Add (Val 4) (Val 7)) (Div (Val 10) (Val 2))
-- expr2 = Sub (Val 10) (Div (Val 14) (Val 0))
-- eval :: MonadError String m => CalcExpr -> m Int
-- The Either type implements the required monadic interface.  Hence we can evaluate using
-- this type as follows:

-- ghci> eval expr1 :: Either String Int
-- Right 55
-- ghci> eval expr2 :: Either String Int
-- Left "Divide by zero!"

-- data CalcExpr = Val Int
--               | Add CalcExpr CalcExpr
--               | Mult CalcExpr CalcExpr
--               | Div CalcExpr CalcExpr
--               | Sub CalcExpr CalcExpr

-- get :: m s
-- put :: s -> m ()
-- modify :: MonadState s m => (s -> s) -> m ()

eval :: MonadError String m => CalcExpr -> m Int
eval (Val x ) = do
    return x
eval (Add x) = do
    modify(+x)
    eval x



eval (Div x y) = do
  x' <- eval x
  y' <- eval y
  if y' == 0
    then throwError "Divide by zero!"
    else return (x' `div` y')

-- Now let's imagine a calculator with an integer state which allows the user to update this state using
-- commands. Here is a data type describing a list of commands:

-- data CalcCmd = EnterC
--              | StoreC Int CalcCmd
--              | AddC Int CalcCmd
--              | MultC Int CalcCmd
--              | DivC Int CalcCmd
--              | SubC Int CalcCmd


-- which runs the given sequence of commands in any monad supporting state and exceptions.
-- Each of the AddC, MultC, DivC and SubC commands should apply
-- the corresponding operation on the provided argument and whatever
-- the current state is.  The StoreC command manually updates the
-- state. Finally, EnterC terminates the calculation, returning the
-- unit type.

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
-- type CS a = StateT Int (Either String) a
run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC = do
    return ()
run (StoreC x c) = do
    put x
    run c
    return ()
run (AddC x c) = do
    modify (+x)
    run c
    return ()
run (MultC x c) = do
    modify (*x)
    run c
    return ()
run (DivC x c) = do
    modify (div x)
    if x == 0 
    then throwError "Divide by zero!" 
    else modify (div x)
    run c
    return ()
run (SubC x c) = do
    modify (-x)
    run c
    return ()



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


-- Write this as a Haskell function using the state monad:

-- factorial :: Integer -> Integer
-- factorial n = snd (runstate (facHelper n) 1)


-- Hint: See how we define fib' using the state monad in the monads handout.
facHelper :: Integer -> State Integer ()
facHelper n = do
    y <- get
    if n > 1
    then do
        put (y * n)
        facHelper (n - 1)
    else return ()
//

headMaybe :: [a] -> Maybe a
headMaybe [] = Noting
headMaybe (x:xs) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (x:xs) = Just xs 

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _ = Just []
takeMaybe _ [] = Nothing
takeMaybe n (x:xs) = 
    case takeMaybe (n-1) xs of
        Nothing -> Nothing
        Just ys -> Just (x:ys)

zipEither :: [a] -> [b] -> Either String [(a,b)]
zipEither [] [] = Right []
zipEither (x:xs) (y:ys) = 
    case zipEither xs ys of
        Left s -> Left s
        Right zs -> Right ((x,y):zs)

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
			   deriving (Show, Read, Eq, Ord, Enum)

data WorkingDay = Mon' | Tue' | Wed' | Thu' | Fri'
               deriving (Show, Read, Eq, Ord, Enum)


toWeekDay :: WorkingDay -> WeekDay
toWeekDay Mon' = Mon
toWeekDay Tue' = Tue
toWeekDay Wed' = Wed
toWeekDay Thu' = Thu
toWeekDay Fri' = Fri


toWorkingDay :: WeekDay -> WorkingDay
toWorkingDay Mon = Mon'
toWorkingDay Tue = Tue'
toWorkingDay Wed = Wed'
toWorkingDay Thu = Thu'
toWorkingDay Fri = Fri'
toWorkingDay Sat = error "Not a working day"
toWorkingDay Sun = error "Not a working day"


toList :: Maybe a -> [a]
toList Nothing = []
toList (Just x) = [x]


toMaybe :: [a] -> Maybe a
toMaybe [] = Nothing
toMaybe (x:xs) = Just x


data BinLN a b = Leaf a | Node (BinLN a b) b (BinLN a b)

leaves :: BinLN a b -> [a]
leaves (Leaf x) = [x]
leaves (Node l _ r) = leaves l ++ leaves r

data BinL a = Lf a | Nd (BinL a) (BinL a)

showBin :: Show a => BinL a -> String
showBin (Lf x) = show x
showBin (Nd l r) = "(" ++ showBin l ++ " " ++ showBin r ++ ")"

data BT a = Empty | Ne a (BT a) (BT a)
            deriving (Show, Read, Eq)
(//) :: BT a -> BT a -> BT a 
(//) Empty s = s
(//) (Ne x l r) s = Ne x l (r // s) 

data Json = JNull
	  | JStr String
	  | JNum Float
	  | JBool Bool
	  | JArr [Json]
	  | JObj [(String, Json)]


so that allValues json key recursively finds all values associated with the string `key` occuring in instances of the `JObj`constructor.
allValues :: Json -> String -> [Json]
allValues (JObj []) _ = []


