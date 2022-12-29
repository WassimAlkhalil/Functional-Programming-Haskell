import Test.QuickCheck

-- Author: Wassim Alkhalil

-- Task 1
-- the symbole -> separates the function argument from the function body
-- the namless function x takes an integer and returns a function that takes an integer and the function body is the sum of the two integers.
add :: Int -> (Int -> Int)
add = (\x -> (\y -> x + y))
    ---------------------
myLength :: [a] -> Int
myLength xs = sum [1 | x <- xs]
    ---------------------
odds :: Int -> [Int]
odds n = [i | i <- [1..n], i `mod` 2 == 1]
    ---------------------
quadruple :: Int -> Int
quadruple x = mult 2 x * 2
            where mult 0 _ = 0
                  mult n m = m + mult (n-1) m
--------------------------------
-- Task 2
    ----------
-- to generate a list of integers that decreases in value using the [x .. y] syntax in Haskell will result always an empty list. [99 .. 1] = [] 
-- but, the [3,2 .. -3] expression will result [3,2,1,0,-1,-2,-3].
    ----------
-- CONTRACT
repeatn :: Int -> [a] -> [a]
-- PURPOSE
-- repeatn takes an Integer and a list of any type and returns a list of any type. 
-- each element of the list will be repeated n times.
-- EXAMPLES
example_repeatn_1 = repeatn 3 [1,2,3] == [1,1,1,2,2,2,3,3,3]
-- DEFINITION
repeatn n xs = [x | x <- xs, i <- [1..n]]
-- TESTS
prop_repeatn_repeat_0 n xs = repeatn n xs == concat[[x |x <- xs, i <- [1..n]]] 
prop_repeatn_repeat_1 n xs = repeatn n xs == concat[helper x | x <- xs]  
                                                     where 
                                                        helper x = take n (repeat x)                              
    ---------------------                       
-- CONTRACT
repeatc :: Int -> [a] -> [a]
-- PURPOSE
-- repeatc takes an Integer and a list of any type and returns a list of any type.
-- EXAMPLES
example_repeatc_0 = repeatc 3 [1,2,3] == [1,2,3,1,2,3,1,2,3]
example_repeatc_1 = repeatc 3 ['a','b','c'] == "abcabcabc"
-- DEFINITION
repeatc n xs = [x | i <- [1..n], x <- xs]
-- TEST
prop_repeatc_repeat n xs = repeatc n xs == concat(replicate n xs)
--------------------------------
-- Task 3
-- CONTRACT
factors :: Int -> [Int]
-- PURPOSE
-- the function factors takes an integer and returns a list of integers.
-- EXAMPLE
example_factors_0 = factors 10 == [1,2,5,10]
example_factors_1 = factors 11 == [1,11]
-- DEFINITION
factors n = [x | x <- [1..n], n `mod` x == 0]
-- TEST
prop_factors_factors n = factors n == [x | x <- [1..n], n `mod` x == 0]
    ---------------------
-- CONTRACT
prime :: Int -> Bool
-- PURPOSE
-- the function prime takes an integer and returns a boolean value.
-- EXAMPLE 
example_prime_0 = prime 10 == False
example_prime_1 = prime 11 == True
-- DEFINITION
prime n = factors n == [1,n]
-- Test 
prop_prime_prime n = prime n == (factors n == [1,n])
    ---------------------
-- CONTRACT
numPrimes :: Int -> Int
-- PURPOSE
-- the function numPrimes determines the number of primes smaller than or equal to its argument.
-- EXAMPLE
example_numPrimes_0 = numPrimes 8 == 3
example_numPrimes_1 = numPrimes 5 == 3 
-- DEFINITION
numPrimes x = length[ i | i <- factors x, i <= x]
-- or we can use : numPrimes x = length[ i | i <- [1..x], prime i]
-- TEST
prop_numPrimes_length x = numPrimes x == length[ i | i <- factors x, i <= x]
--------------------------------
-- Task 4
-- CONTRACT
scalarProduct :: [Int] -> [Int] -> Int
-- PURPOSE
-- the function scalarProduct takes two integer lists of the same length and calculates the sum of the products. 
-- EXAMPLES
example_scalarProduct_0 = scalarProduct [1,2,3] [1,2,3] == 14
example_scalarProduct_1 = scalarProduct [1,2,3] [1,2,3,4] == error "the length of the first list is not equal to the length of the second list"
-- DEFINITION
scalarProdut [] [] = 0
scalarProduct _ [] = 0
scalarProduct [] _ = 0
scalarProduct xs ys = if length xs /= length ys then error "the length of the first list must be equal to the length of the second list" else sum[x*y | (x,y) <- zip xs ys]
-- TEST
prop_scalarProduct_calculation xs ys = scalarProduct xs ys == product[x*y | x <- xs, y <- ys]                                                                                                                                                                                       
--------------------------------
-- Task 5
-- CONTRACT
nestedRemoveEven :: [[Int]] -> [[Int]]
-- PURPOSE
-- nestedRemoveEven is a function that takes a list of lists of integers and returns a list of lists of integers.
-- the function removes all even numbers and returns a list of lists that contain only the odd numbers. 
-- EXAMPLES
example_nestedRemoveEven_0 xss = nestedRemoveEven [[1,2,3],[4,5,6],[7,8,9]] == [[1,3],[5],[7,9]]
example_nestedRemoveEven_1 xss = nestedRemoveEven [[-1,3,2],[4,-5,-6]] == [[-1,3], [-5]]
-- DEFINITION
nestedRemoveEven xss = [filter odd xs | xs <- xss]
-- TEST
prop_nestedRemoveEven_filter xss = nestedRemoveEven xss == [helper xs | xs <- xss]
                                                                where 
                                                                    helper xs = [x | x <- xs, x `mod` 2 == 1]