-- Author : Wassim Alkhalil

import Data.Char (isUpper)
import Data.List(sort)

-- Task 1
--  let pattern = input in output matches the expression input against pattern and returns the value of output.
{-
a. let [x] = 1 : [] in x
output : 1
    -------------
b. let [x] = "1" in x
output : '1'
    -------------
c. let [x] = 1 in x
output : Error, pattern does not match
    -------------
d. let (x:xs) = [1..10] in (x, xs)
output : (1,[2..9])
    -------------
e. let (x:(y:ys)) = [[1,2],[3,4,5]] in (x, y, ys)
output : ([1,2],[3,4,5],[])
    -------------
f. let (x:xs) = [1,2]:[3,3,3,3] in x
output : Error, we cant use the cons operator for two lists 
    -------------
g. let [x, 2] = [2..3] in x
output : Error, pattern does not match [x,2] /= [2..3]
    -------------
-}
----------------------------------------------------
-- Task 2
myHead :: [a] -> a
myHead [] = error "not valid, the list does not contain any elements"
myHead (x:xs) = x
    -------------
-- myData is a function that takes a list of Integers and returns a list of Integers
-- the function will only read the list of Integers
myData ::[Int] 
myData = read "[1,2,3]"
    
applyFtwice :: (Int -> Int) -> Int -> Int
applyFtwice f x = f (f x)
----------------------------------------------------
-- Task 3
-- a.
myAnd :: Bool -> Bool -> Bool
myAnd True True = True 
myAnd   _   _   = False 
-- Patterns must not repeat variables e.g. myAnd b b = b 
    -------------
-- b.
multComplex :: (Floating a) => (a,a) -> (a,a) -> (a,a) 
multComplex (x1,y1) (x2,y2) = ((x1 * x2) + (-1)*(y1 * y2) , (y1 * x2) + (x1 * y2))
----------------------------------------------------
-- Task 4.
isCapital :: Char -> Bool
isCapital = isUpper
    -------------
average :: (Fractional a, Num a) => [a] -> a
average xs = sum xs / fromIntegral(length xs)
    -------------
getMax :: (Ord a) =>[a] -> a
getMax [] = error "not valid, the list does not contain any elements"
getMax (x:xs) = maximum xs 
-- or getMax (x:xs) = last(sort xs)
-- or getMax (x:xs) = head(reverse(sort xs))
    -------------
det :: (Num a) => [[a]] -> a
det [[a]] = a
det [[a,b],[c,d]] = (a*d) - (b*c)
----------------------------------------------------
-- Task 5.
-- A function is called polymorphic if it can be applied to arguments of different types.
-- A polymorphic function is overloaded if its type contains one or more class constraints.
-- A function that takes its arguments one at a time is called a curried function.
double :: (Num n) => n -> n 
double = mult 2
            where 
                mult x y = x * y
-- double is a function that takes a single argument of type n and returns the result of multiplying that argument by 2. 
-- It does this by using a technique called currying, which involves defining a function that takes multiple arguments as a series of functions that each take a single argument. 
-- The mult function is defined to take two arguments and return their product, and the definition of double uses mult with only one argument and assigns the resulting function to double. 
-- When double is called with a single argument, it uses that argument as the second argument to mult and returns the product of the two arguments. 
----------------------------------------------------
-- Task 6.
verboseFlip :: (a -> b -> c) -> (b -> a -> c)
verboseFlip f = g
    where 
        g x y = f y x
-- verposeFlip is a function that takes a function of type (a -> b -> c) and returns a function of type (b -> a -> c).
-- The function that is returned takes two arguments and applies them to the original function in the opposite order.
    -------------
curryFlip :: (a -> b -> c) -> b -> a -> c
curryFlip f x y = f y x
-- curryFlip is a function that takes a function of type (a -> b -> c) and returns a
-- The function that is returned takes two arguments and applies them to the original function in the opposite order.
----------------------------------------------------