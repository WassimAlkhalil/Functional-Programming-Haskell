-- Author : Wassim Alkhalil
---------------------------
-- Task 1
---------
{-
a. take 3 ['P','F','f','u','n']
"nuf"

:t take 
take :: Int -> [a] -> [a]
        --------------
b. reverse ['P','F','f','u','n'] 
"nufFP"

:t reverse
reverse :: [a] -> [a]
        --------------
c. ['n','u','f','F','P'] == "nufFP"
True 

:t (==)
(==) :: Eq a => a -> a -> Bool
        --------------
d. head ['n','u','f','F','P']
'n'

:t head
head :: [a] -> a
        --------------
e. head(reverse ['n','u','f','F','P'])
   last ['n','u','f','F','P']
   head (drop 4 ['n','u','f','F','P'])
'P'

:t head
head :: [a] -> a
:t reverse
reverse :: [a] -> [a]
:t last
last :: [a] -> a
:t drop 
drop :: Int -> [a] -> [a]
        --------------
f. drop 1 ['n','u','f','F','P']
"ufFP"

:t drop 
drop :: Int -> [a] -> [a]
        --------------
g. drop 3 ['n','u','f','F','P']
"FP"

:t drop
drop :: Int -> [a] -> [a]
        --------------
h. drop 2 (reverse ['n','u','f','F','P'])
"nuF"

:t drop
-}
  ---------------------------------------------
-- Task 2
---------
xs = [4,1] 
ys = [3,2]

quickSort :: [Int] -> [Int]
-- Base case 
quickSort [] = []
-- Definition 
quickSort (x:xs) = quickSort ms ++ [x] ++ quickSort ns
                  where 
                        ms = [ a | a <- xs, a <= x ]
                        ns = [ b | b <- xs, b > x ]
-- first way
zs = drop 1 xs ++ drop 1 ys ++ take 1 ys ++ take 1 xs  
-- second way 
ns = quickSort(xs ++ ys)
-- third way
hs = drop 1 xs ++ take 2 (reverse(xs ++ ys)) ++ take 1 (xs ++ ys) 
  ---------------------------------------------
-- Task 3 
---------
-- we can use the short form [3,6 .. 99]

-- list comprehension
divisibality_1 :: [Int] -> [Int]
divisibality_1 xs = [x | x <- xs, x `mod` 3 == 0]

-- recursion
divisibality_2 :: [Int] -> [Int]
divisibality_2 [] = []
divisibality_2 (x:xs) | x `mod` 3 == 0 = x : divisibality_2 xs
                      | otherwise = divisibality_2 xs
  ---------------------------------------------
-- Task 4
---------
-- a.
-- [] represents empty list
-- [[]] represents a list with an empty list as its only element

-- b. 
{-
reverse []
[]
The reason for this is that an empty list does not have any elements, so there is nothing to reverse. Therefore, the result of reverse [] is simply an empty list, regardless of the type of the elements in the list.
:t reverse
reverse :: [a] -> [a]
        --------------
head []
*** Exception: Prelude.head: empty list
This is because an empty list does not have any elements, so there is nothing for the head function to return. As a result, an error message is displayed when you try to use head on an empty list.
:t 
head :: [a] -> a
        --------------
sum []
0
The reason for this is that an empty list does not have any elements, so there is nothing to add together. Therefore, the result of sum [] is simply 0, regardless of the type of the elements in the list.
:t sum
sum :: Num a => [a] -> a
        --------------
-}
