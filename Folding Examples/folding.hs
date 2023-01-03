import Data.List (sort)

                    -- FOLDING HASKELL --
-- Author : Wassim Alkhalil
---------------------------
-- EXPLAINATION  
---------------
-- CONTRACT
-- foldr :: (a->b->b) -> b -> [a] -> b
-- PURPOSE
-- fold right is a higer order function that takes a function as first argument and the function should be a binary function, this function takes two arguments, type a and b and returns something of type b
-- it also takes a second argument from type b and a third argument from type list of and returns something of type b

-- foldr (operator) a [x1,..,xn] = x1 (operation) x2 (operation) ... xn (operation) a
-- how to build a new function ?
-- foldr (\elem acc -> <term>) <start_acc> <list>
                ---------------------------------------------
-- CONTRACT
-- foldl :: (a->b->b) -> b -> [a] -> b
-- PURPOSE
-- fold left is a higher order function that takes a function as first argument and the function should be a binary function, this function takes two arguments, type a and b and returns something of type b
-- it also takes a second argument from type b and a third argument from type list of and returns something of type b
-- foldl (operator) a [x1,..,xn] = x1 (operation) x2 (operation) ... xn (operation) a
-- how to build a new function ?
-- foldl (\acc elem -> <term>) <start_acc> <list>
---------------------------------------------------------------------------------------------------------
-- Task 1
-- Define reverse, which reverses a list, using foldr.
-- CONTRACT
reverse' :: [a] -> [a]
-- PURPOSE
-- reverse is a function that takes a list of type a and returns a list of type a
-- EXAMPLES
example_reverse'_0 = reverse' [1,2,3,4] == [4,3,2,1]
-- DEFINITION
reverse' = foldl (\acc x -> x : acc) [] 
-- other way to define the function : foldl (flip (:)) [] 
                ---------------------------------------------
-- Task 2
-- CONTRACT
prefixes :: [a] -> [[a]]
-- PURPOSE
-- EXAMPLES
example_prefixes_0 = prefixes [1,2,3] == [[1],[1,2],[1,2,3]]
-- DEFINITION
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc)) []
                ---------------------------------------------
-- Task 3
-- Using the higher-order function foldr define a function sumsq which takes an integer n as its argument and returns the sum of the squares of the first n integers. That is to say, sumsq n = 1^(2) + 2^(2) + 3^(2) + .. + n^(2). Do not use the function map.
-- sumsq n = 12 + 22 + 32 + ... + n2. Do not use the function map.
-- CONTRACT
sumsq :: Int -> Int
-- EXAMPLES
example_sumsq_0 = sumsq 3 == 14
-- DEFINTION
sumsq n = foldr (\x acc -> helper x + acc) 0 [1..n]
    where
        helper x = x * x
                ---------------------------------------------
-- Task 4
-- Define length, which returns the number of elements in a list, using foldr. Rede- fine it using foldl.
-- CONTRACT
length' :: [a] -> Int
-- EXAMPLES
example_length_0 = length' [1,2,3,4] == 4
example_length_1 = length' ['a','b'] == 2
-- DEFINITION
{-
length' = foldr (\x acc -> helper x + acc) 0 
            where 
                helper x = 1
-}
length' = foldl (\acc x -> helper x + acc) 0 
            where 
                helper x = 1
                ---------------------------------------------
-- Task 5
-- Define minlist, which returns the smallest integer in a non-empty list of integers, using foldr1. Redefine it using foldl1.
-- CONTRACT
minlist :: [Int] -> Int
-- EXAMPLES
example_minlist_0 = minlist [1,2,3,4] == 1
-- DEFINITION
minlist xs = foldr (\x acc -> helper x) 0 xs 
            where 
                helper x = minimum xs 
-- minlist xs = foldr(\x acc -> sort xs !! 0 + 0) 0 xs
                ---------------------------------------------