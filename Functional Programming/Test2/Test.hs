-- Background Material
-- The Tribonnaci sequence is a generalisation of the Fibonacci sequence defined
-- as follows:
-- T1 = 1,
-- T2 = 1,
-- T3 = 2,
-- Tn = Tn-3 + Tn-2 + Tn-1.
-- A naive implementation can be given as follows:


-- Implementation Task
-- Rewrite the above function using the state monad to improve its efficiency.


-- The function runStateTrib n should return the nth Tribonacci number
-- efficiently, using the state monad to keep track of intermediate values.
-- This function will be tested for large values of n. In particular,
-- runStateTrib 1000 should run in well under 1 second.

-- Examples

-- ghci> runStateTrib 30
-- 29249425
-- (0.00 secs, 106,632 bytes)
-- ghci> runStateTrib 1000
-- 1499952522327196729941271196334368245775697491582778125787566254148069690528296568742385996324542810615783529390195412125034236407070760756549390960727215226685972723347839892057807887049540341540394345570010550821354375819311674972209464069786275283520364029575324
-- (0.00 secs, 1,184,936 bytes)


trib :: Int -> Int
trib 1 = 1
trib 2 = 1
trib 3 = 2
trib n | n > 3 = trib (n-3) + trib (n-2) + trib (n-1)

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib n = do 
    modify(\(a,b,c) -> (a+b+c,a,b))
    StateTrib (n-1)








-- Background
-- Recall the type of binary trees carrying data at both the node and leaves:

-- data Bin a b = Lf a
--              | Nd b (Bin a b) (Bin a b)
--              deriving (Eq, Show)



-- Implementation Task
-- Write a function to perform an in-order traversal of the tree, logging the values of the nodes and leaves using an Either type (since they have different types):





-- Examples
-- Consider the trees

-- tr1 = Nd 'a' (Lf 4) (Nd 'b' (Lf 7) (Lf 2))
-- tr2 = Nd 3 (Nd 5 (Nd 7 (Lf 'a') (Lf 'b')) (Nd 8 (Lf 'c') (Lf 'd'))) (Nd 4 (Lf 'e') (Lf 'f'))


-- Traversing them gives:

-- ghci> snd $ runWriter $ writeLeaves tr1
-- [Left 4,Right 'a',Left 7,Right 'b',Left 2]
-- ghci> snd $ runWriter $ writeLeaves tr2
-- [Left 'a',Right 7,Left 'b',Right 5,Left 'c',Right 8,Left 'd',Right 3,Left 'e',Right 4,Left 'f']



writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf a) = tell [Left a]
writeLeaves (Nd b l r) = writeLeaves l >> tell [Right b] >> writeLeaves r

-- Backgroup Material
-- Refer to the type Bin a b of binary trees from the previous question.

-- Implementation Task
-- Suppose that the leaves of our tree are themselves decorated by binary trees.  Write
-- a function:

-- collapse :: Bin (Bin a b) b -> Bin a b
-- collapse = undefined


-- which converts this to a single tree, promoting the decorations of the leaves to be subtrees of the result.

-- Example
-- Given the tree

-- tr3 :: Bin (Bin Int Char) Char
-- tr3 = Nd 'a' (Lf (Nd 'b' (Lf 2) (Lf 7))) (Lf (Nd 'c' (Lf 2) (Lf 7)))


-- we have

-- ghci> collapse tr3
-- Nd 'a' (Nd 'b' (Lf 2) (Lf 7)) (Nd 'c' (Lf 2) (Lf 7))
data Bin a b = Lf a
             | Nd b (Bin a b) (Bin a b)
             deriving (Eq, Show)

tr3 :: Bin (Bin Int Char) Char
tr3 = Nd 'a' (Lf (Nd 'b' (Lf 2) (Lf 7))) (Lf (Nd 'c' (Lf 2) (Lf 7)))

collapse :: Bin (Bin a b) b -> Bin a b
collapse (Lf (Lf a)) = Lf a
collapse (Lf (Nd b l r)) = Nd b (collapse (Lf l)) (collapse (Lf r))
collapse (Nd b l r) = Nd b (collapse l) (collapse r)

-- Background Material
-- Consider the map function on lists:

-- map :: (a -> b) -> [a] -> [b]


-- As the map proceeds, the function provided as a first argument only has access to the element (of type a) in order to produced an output value of type b.  A slightly more sophisticated map is given by the mapWithIndex function:

-- mapWithIndex :: (a -> Int -> b) -> [a] -> [b]


-- In this version, the first argument is a function which is passed not only the current element as the map proceeds but also the index of that element in the list.
-- We can do something completely analotous for trees.  Referring to the binary trees from Question 2, recall that each node or leaf of the tree can be assigned an adress (see the section "Directions, adresses and paths in binary trees" from User defined data types I) consisting of the list of choices of moving left and right required to reach it.  That is:

-- data Direction = L | R deriving (Eq, Show)
-- type adress = [Direction]

-- which applies the provided function to the data at each leaf, while also passing in the adress of that leaf.

-- Examples
-- Consider again the trees

-- tr1 = Nd 'a' (Lf 4) (Nd 'b' (Lf 7) (Lf 2))
-- tr2 = Nd 3 (Nd 5 (Nd 7 (Lf 'a') (Lf 'b')) (Nd 8 (Lf 'c') (Lf 'd'))) (Nd 4 (Lf 'e') (Lf 'f'))


-- One possible thing we could do is to simply return the adress when we reach a leaf.  In this case, each leaf will become decorated by its own adress.  For example:

-- ghci> mapLeavesWithadress (\ _ adr -> adr) tr1
-- Nd 'a' (Lf [L]) (Nd 'b' (Lf [L,R]) (Lf [R,R]))
-- ghci> mapLeavesWithadress (\ _ adr -> adr) tr2
-- Nd 3 (Nd 5 (Nd 7 (Lf [L,L,L]) (Lf [R,L,L])) (Nd 8 (Lf [L,R,L]) (Lf [R,R,L]))) (Nd 4 (Lf [L,R]) (Lf [R,R]))


data Direction = L | R deriving (Eq, Show)
type adress = [Direction]

mapLeavesWithadress :: (a -> adress -> c) -> Bin a b -> Bin c b
mapLeavesWithadress = toadr []
    where 
        
        toadr adr f (Lf a) = Lf (f a adr)
        toadr adr f (Nd b l r) = Nd b (toadr (adr ++ [L]) f l) (toadr (adr ++ [R]) f r)

        

Background
A QuadTree is a data structure which is very useful for storing images.  It is a tree in which each node has 4 descendants, corresponding to `div`iding an image into 4 regions, which we will label by NW, NE, SW and SE.  Using integers to represent pixels, the definition would look like this:

type Pixel = Integer

data QuadTree = N QuadTree QuadTree QuadTree QuadTree
              | P Pixel
              deriving (Eq, Show)


For a 2x2 image, here's a picture of the corresponding quadtree.  It consists of a single node with 4 leaves as children.

The labels NW, NE, SW and SE here tell us which parts of the image are stored in which constructor arguments.  The displayed quadtree would be written N (L NW) (L NE) (L SW) (L SE) (of course, the labels such as NW are not integers so this expression is not well typed.  It's just to fix an ordering convention).
Now, for a 4x4 image, we sub`div`ide again, leading to a two level tree like this:

On the other hand, we can represent an image as a 2-dimensional array, implemented here as a list of lists:

type Image = [[Pixel]]


Here are two example images (these definitions are imported for you in the template):

image1 = [ [1, 2]
         , [3, 4] ]

image2 = [ [1,  2,  3,  4]
         , [5,  6,  7,  8]
         , [9,  10, 11, 12]
         , [13, 14, 15, 16] ]



-- Implementation Task
-- Write two functions:

-- which converts an Image to a QuadTree, and vice-versa.
-- For simplicity, you may assume that the images we want to convert are

-- Non-empty, and
-- Square arrays where the length of each side are powers of two. This means that

-- The resulting QuadTree's have uniform depth (as show in the two examples)
-- All of the Leaves are on the same level. Hence, any Node which contains a Pixel will only contain Pixel's.



-- Your code does not need to handle images or trees which are not of this form, and may simply leave these cases undefined.
-- To obtain full marks, your functions must be mutually inverse. That is, we should have

-- fromQuadTree (toQuadTree im) == im
-- toQuadTree (fromQuadTree qt) == qt


-- for any image im and any quadtree qt satisfying the stated conditions.

-- Examples

-- ghci> toQuadTree image2
-- N (N (P 1) (P 2) (P 5) (P 6)) (N (P 3) (P 4) (P 7) (P 8)) (N (P 9) (P 10) (P 13) (P 14)) (N (P 11) (P 12) (P 15) (P 16))

-- ghci> fromQuadTree (N (N (P 1) (P 2) (P 5) (P 6)) (N (P 3) (P 4) (P 7) (P 8)) (N (P 9) (P 10) (P 13) (P 14)) (N (P 11) (P 12) (P 15) (P 16)))
-- [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

-- ghci> image2 == fromQuadTree (toQuadTree image2)
-- True

-- ghci> image1 == fromQuadTree (toQuadTree image2)

toQuadTree :: Image -> QuadTree
toQuadTree [[a]] = P a
toQuadTree im = N (toQuadTree (nw)) (toQuadTree (ne)) (toQuadTree (sw)) (toQuadTree (se))
   where nw = map (take (length im `div` 2)) (take (length im `div` 2) im)
         ne = map (drop (length im `div` 2)) (take (length im `div` 2) im)
         sw = map (take (length im `div` 2)) (drop (length im `div` 2) im)
         se = map (drop (length im `div` 2)) (drop (length im `div` 2) im)

fromQuadTree :: QuadTree -> Image
fromQuadTree (P a) = [[a]]
fromQuadTree (N nw ne sw se) = zipWith (++) (fromQuadTree nw) (fromQuadTree ne) ++ zipWith (++) (fromQuadTree sw) (fromQuadTree se)