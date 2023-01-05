-- Contract
mySplitAt :: Int -> [a] -> ([a], [a])

-- Purpose
--  mySplitAt n xs returns a tuple where first element is the length n prefix of
--  the list xs, and the second element is the remainder of the list xs.

-- Examples
example_mySplitAt_1 = mySplitAt 1 [1,2,3] == ([1],[2,3])
example_mySplitAt_2 = mySplitAt 4 [1,2,3] == ([1,2,3],[])
example_mySplitAt_3 = mySplitAt (-1) [1,2,3] == ([],[1,2,3])

-- Definition
mySplitAt n xs | n <= 0 = ([], xs)
mySplitAt _ [] = ([], [])
mySplitAt n (x : xs) = let (ys,zs) = mySplitAt (pred n) xs in (x:ys, zs)

-- Tests
prop_mySplitAtKeep n xs = let (ps,qs) = mySplitAt n xs in ps ++ qs == xs
    where types = xs :: [Int]
prop_mySplitAtLength :: Int -> [Int] -> Bool
prop_mySplitAtLength n xs = length (fst (mySplitAt n xs)) == expected
    where expected = min (max 0 n) (length xs)
prop_mySplitAtDropTail n xs = as == take n' xs && bs == drop n' xs
    where n' = n `mod` succ (length xs)
          (as, bs) = mySplitAt n' xs
          _ = xs :: [Int]
