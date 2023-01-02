import Test.QuickCheck
import Prelude hiding (map)
-- Author: Wassim Alkhalil
-- Task 1

-- CONTRACT
isOdd :: (a -> Bool) -> [a] -> Bool

-- PURPOSE
-- isOdd is a function that takes a predicate and a list and returns a boolean.
-- the function takes the list xs and checks if there is an odd number in the list.

-- EXAMPLES
example_isOdd_1 = isOdd odd [] == False
example_isOdd_0 = isOdd odd [1,2,3,4,5] == True

-- DEFINITION
isOdd _ [] = False 
isOdd odd (x:xs) = odd x || isOdd odd xs

-- TEST
prop_isOdd_odd xs = isOdd odd xs == or [ odd x | x <- xs]
-- if a function has type Foldable t => (a -> Bool) -> t a -> Bool
-- then we can treat it as if it had the type (a -> Bool) -> [a] -> Bool
-- since Foldable t => t a is the same as [a]
        ---------------------
-- Task 2

-- a.
{-
any (++"e") "Test1"
Couldn't match type ‘[Char]’ with ‘Bool’
       * the function any takes a predicate and a list and returns a boolean. the concatination does not return a boolean.
Couldn't match type ‘Char’ with ‘[Char]’
       * the concatination does not work since 'T' ++ "e" have diffrent types. 
correct version : 
any (=='e') "Test1"
any (=='e') ['T','e','s','t','1']
it will check if there is any character in the list that matches with the character 'e' 
-}
        ---------------------
-- b.
{-
all (/='e') "Test2"
returns False, since not all the characters in the list are not equal to 'e'
map (all (/='e')) "Test2"
Couldn't match type ‘Char’ with ‘t0 Char’
        * we can't use map since the function all takes a predicate and a list and returns a boolean.
        * the function map takes a function and a list and returns a list.
        * the function all takes a predicate and a list and returns a boolean.
        * the function map takes a function and a list and returns a list.
correct version :
map (/='e') "Test2"
-}
        -------------------------------------------------       
-- Task 3 

-- CONTRACT
double :: [Int] -> [Int]

-- PURPOSE
-- double is a function that takes a list of integers and returns a list of integers.
-- the function takes the even numbers and double each even number in the list. 

-- EXAMPLES
example_double_0 = double [1,2,3,4,5] == [4,16]
example_double_1 = double [] == []

-- DEFINITION
double xs = map (\x -> x * x) [x | x <- filter even xs ]
-- or we can use : double xs = [ x * x | x <- filter even xs] 

-- TESTS
prop_double_square xs = double xs == [ x*x | x <- xs, even x ]
        -------------------------------------------------
-- Task 4
-- a.
-- CONTRACT
foo :: [Int] -> [Int]
-- PURPOSE
-- foo is a function that takes a list of integers and returns a list of integers.
-- the function takes the list xs and add 2 to each element in the list.
-- EXAMPLES
example_foo_0 = foo [1,2,3,4,5] == [3,4,5,6,7]
example_foo_1 = foo [] == []
-- DEFINITION
foo xs = map (+1) (map (+1) xs)
-- TESTS
prop_foo_map xs = foo xs == [ y | x <- xs, let y = x + 2]
                ---------------------
-- b.
-- COMTRACT
bar :: [Int] -> Int
-- PURPOSE 
-- EXAMPLES 
example_bar_0 = bar [1..20] == 5
example_bar_1 = bar [] == 0
-- DEFINITION
bar xs = sum (map (\_ -> 1) (filter (>7) (filter (<13) xs)))
-- TEST
prop_bar_counter xs = bar xs == sum[ 1 | x <- xs, x > 7 && x < 13]
                ---------------------
-- c.
-- CONTRACT 
baz :: [[Int]] -> [[Int]]
-- PURPOSE
-- baz is a function that takes a list of lists of integers and returns a list of lists of integers.
-- the function takes the list of lists xs and add 1 to each element in the list.
-- EXAMPLES
example_baz_0 = baz [[]] == [[]]
example_baz_1 = baz [[1,2,3], [4,5,6]] == [[2,3,4],[5,6,7]]
-- DEFINITION
baz xss = map (map (+1)) xss
-- TESTS
prop_baz_map xss = baz xss == [ helper xs | xs <- xss]
                                        where 
                                           helper xs = map(+1) xs
        -------------------------------------------------
-- Task 5
-- a.
{-
foldl (-) 0 [1,2,3]
foldl (-) (0-1) [2,3]
foldl (-) ((0-1)-2) [3]
foldl (-) (((0-1)-2)-3) []
(((-1)-2)-3)
((-3)-3) 
-6
-}
                ---------------------
-- b.
{-
foldr (-) 0 [1,2,3]
1 - (foldr [2,3])
1 - (2 - (foldr [3]))
1 - (2 - (3 - foldr []))
1 - (2 - (3 - 0))
2

-- or we can do it in this way :

foldr (-) 0 [1,2,3]
foldr (-) 0 (1:(2:(3:[])))
we replace each (:) by (-) and the empty list [] by 0
(1-(2-(3-0)))
2
-}
        -------------------------------------------------
-- Task 6
{-
foo xs = map (+1) $ map (+1) xs
bar xs = sum $ map (\_ -> 1) $ filter (> 7) $ filter (< 13) xs
baz xss = map $ map (+1) xss
-}
        -------------------------------------------------
-- Task 7
-- CONTRACT
map :: (a -> b) -> [a] -> [b]

-- PURPOSE 
-- map is a function that takes a function and a list and returns a list.
-- the function takes a function f and a list xs and applies the function f to each element in the list xs.

-- EXAMPLES
example_map_0 = map (+1) [] == []
example_map_1 = map (+1) [1,2,3,4] == [2,3,4,5]

-- DEFINITION
map f xs = foldr (\x acc -> f x : acc) [] xs
