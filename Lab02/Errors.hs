import Test.QuickCheck
import Data.List (sort)

-- Author : Wassim Alkhalil

-- Task 1
next :: Int -> Int
next x = x + 1

prev :: Int -> Int
prev x = - 1 + x

tupleup :: Int -> (Int, (Int, Int))
tupleup i = (i, (i, i))

myNameIs :: [Char] -> Bool
myNameIs str = str == "Nobody"

newLine :: String -> String
newLine str = str ++ "\n"
-------------------------------------------------
-- Task 2
{-
*Main> :t ['1','2','3']
['1','2','3'] :: [Char]
    ------------------
*Main> :t (["False", "True"], [False, True], ['0','1'])
(["False", "True"], [False, True], ['0','1']) :: ([[Char]], [Bool], [Char])
    ------------------
*Main> :t [1] ++ ['a']
Error : We cannot concatinate two lists fro diffrent types and therefor the type is not defined
    ------------------
*Main> :t [(False, '0'), (True, '1')]
[(False, '0'), (True, '1')] :: [(Bool,Char)]
    ------------------
*Main> :t [("False", '0'), ("True", '1')]
[("False", '0'), ("True", '1')] :: [([Char], Char)]
    ------------------
*Main> :t ("1, 2",("3"))
("1, 2",("3")) :: ([Char], [Char])
    ------------------
*Main> :t [['1'],("True")]
[['1'],("True")] :: [[Char]]   
    ------------------
*Main> :t [tail, init, reverse]
[tail, init, reverse] :: [[a] -> [a]]
-}
-------------------------------------------------
-- Task 3 
-- a. 
{-
  a   : is a type variable, which means that it can represent any type. 
 'a'  : is a character, which is a type that represents a single Unicode character. 
['a'] : is a list of characters.
 "a"  : is a string, which is a type that represents a sequence of characters
-}   
    ------------------
-- b.
{-
In Haskell, not 'a' is a type error because not is a function that takes a boolean value as an argument, 
but 'a' is a character, not a boolean. This causes a type mismatch, as the expected input type for not is Bool, 
but the actual type of the argument passed to not is Char. The error message for this type of error might look something like this: Couldn't match expected type Bool' with actual type Char'.
-}
    ------------------
-- c.
foo :: Char -> String
foo bar = [bar]

-- To determine the type of (foo bar), where foo is a function of type Char -> String and bar is a value of type Char, you can look at the type signature of the function foo. 
-- The type of (foo bar) will be the return type of the function, which in this case is String.
-------------------------------------------------
-- Task 4
-- COMTRACT
square :: Int -> Int
-- PURPOSE 
-- square is a function that takes an integer and returns the square of that integer
-- EXAMPLES
square (-1) = 1
square 0 = 0
square 1 = 1 
square 2 = 4
-- DEFINITION
square n = n * n
-- TESTS
prop_square n = square n == n^2
-------------------------------------------------
-- Task 5   
-- COMTRACT
qsort :: [Int] -> [Int]
-- PURPOSE
-- qsort is a function that takes a list of numbers and returns a sorted list of numbers
-- EXAMPLES
qsort [2,3,2,1,2,2,1] = [1,1,2,2,2,2,3]
-- DEFINITION
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b > x]
-- replacing <= by < in qsort will ignore any numbers equal to x, only numbers less or equal than x will be considered.
-- TESTS
prop_qsort_sorting_algorithm xs = qsort xs == sort xs
-------------------------------------------------
