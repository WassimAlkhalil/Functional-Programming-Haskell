import Debug.Trace

-- Task 1
(&.&) :: Bool -> Bool -> Bool
x &.& y = (seq) y (if x 
                        then y 
                               else False)

-- Task 2
sumTwo :: (Num n, Ord n) => [n] -> n
sumTwo (a:b:c:d:ds)
    | a + b > c + d = a + b
    | otherwise     = c + d

-- Task 4
-- a)
-- foo x y = bar x $ baz y
-- foo = bar . baz 
-- b)
reverse' :: [a] -> [a]
reverse' xs = foldr (\x acc -> acc ++ [x]) [] xs

-- Task 5
data Set a = Leaf -- Empty set
           | Node {
                left :: (Set a) -- Set with elements smaller than x
                ,elem :: a -- One element (x)
                ,right :: (Set a) -- Set with elements larger than x
            } deriving (Ord, Eq, Show)

member :: Ord a => a -> Set a -> Bool
member x Leaf = False
member x (Node left y right)
    | x == y    = True
    | x < y     = member x left
    | otherwise = member x right
