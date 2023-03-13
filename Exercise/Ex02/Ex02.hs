module Ex02 where

-- import Test.QuickCheck

-- The following line is required for the functions foo and bar below
import Data.Char

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
euclDistance :: (Float, Float) -> (Float, Float) -> Float
-- PURPOSE [TODO]
-- calculates the distance between two points
-- EXAMPLES [TODO]
example_euclDistance_0 = euclDistance (2.0, 1.0)(2.0, 3.0) == 2.0
example_euclDistance_1 = euclDistance (3.0, 4.0)(3.0, 1.0) == 3.0
-- DEFINITION [TODO]
euclDistance (x0, y0) (x1, y1) = sqrt((x1 - x0)^2 + (y1 - y0)^2)

-- TESTS [TODO]
prop_euclDistance0_singleton = euclDistance (3.0, 1.0) (3.0, 2.0) == 1.0
prop_euclDistance1_singleton = euclDistance (2.0, 2.0) (2.0, 2.0) == 0.0
prop_euclDistance2_singleton = euclDistance (1.0, 2.0) (1.0, 1.0) == 1.0

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
iff :: Bool -> Bool -> Bool

-- PURPOSE [TODO]
-- This function returns True if x and y are equal
-- EXAMPLES [TODO]
example_iff_0 = iff True True
example_iff_1 = not (iff True False)
-- DEFINITION [TODO]
iff = head [iffC, iffG, iffP, iffB]

-- TESTS [TODO]
prop_iff_eq x y = iff x y == (x == y)

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
iffC :: Bool -> Bool -> Bool

-- DEFINITION [TODO]
iffC x y = if x == y then True else False

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
iffG :: Bool -> Bool -> Bool

-- DEFINITION [TODO]
iffG x y | x == y = True
         | otherwise = False

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
iffP :: Bool -> Bool -> Bool

-- DEFINITION [TODO]
iffP True b = b
iffP False True = False
iffP False False = True


--------------------------------------------------------------------------------
-- CONTRACT [TODO]
iffB :: Bool -> Bool -> Bool

-- DEFINITION [TODO]
iffB x y = x && y || not x && not y


--------------------------------------------------------------------------------
-- CONTRACT [TODO]
myTail :: [a] -> [a]

-- PURPOSE [TODO]
-- the function behaves like tail except myTail [] returns []
-- EXAMPLES [TODO]
example_myTail_0 = myTail [1,2,3] == [2,3]
example_myTail_1 = myTail []

-- DEFINITION [TODO]
myTail = head [myTailCond, myTailGuard, myTailMatch]

-- TESTS
prop_TailShrink :: String -> Bool
prop_TailShrink xs = length xs >= length (myTail xs :: String)
prop_TailEquivalent xs = cs == ms && cs == gs
  where cs = myTailCond xs :: [Int]
        ms = myTailMatch xs
        gs = myTailGuard xs

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
myTailCond :: [a] -> [a]

-- DEFINITION [TODO]
myTailCond xs  = if null xs then [] else tail xs

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
myTailGuard :: [a] -> [a]

-- DEFINITION [TODO]
myTailGuard (x:xs) | null xs = []
                   | otherwise = xs

--------------------------------------------------------------------------------
-- CONTRACT [TODO]
myTailMatch :: [a] -> [a]

-- DEFINITION [TODO]
myTailMatch [] = []
myTailMatch [x] = []
myTailMatch (x:xs) = xs
--------------------------------------------------------------------------------
-- CONTRACT
lowerChar :: String -> String

-- PURPOSE [TODO]
-- The function toLower convert a letter to the corresponding lower-case letter, if any. Any other character is returned unchanged.
-- lowerChar implement the function toLower and use the cons operator to map it to the tail of the input.

-- EXAMPLES [TODO]
example_lowerChar_0 = lowerChar "Hello" == "hello"
example_lowerChar_1 = lowerChar "World" == "world"
-- DEFINITION
lowerChar [] = []
lowerChar(x:xs) = toLower x : lowerChar xs

-- TESTS
prop_fooIdempotent xs = lowerChar xs == lowerChar (lowerChar xs)
prop_fooOrderPreserving xs = lowerChar xs == reverse (lowerChar (reverse xs))
--------------------------------------------------------------------------------
-- CONTRACT
upperChar :: String -> String

-- PURPOSE [TODO]
-- the functiom toUpper Convert a letter to the corresponding upper-case letter, if any. Any other character is returned unchanged.
-- upperChar implement the function toUpper and use the cons operator to map it to the tail of the input.
-- EXAMPLES [TODO]
example_upperChar_0 = upperChar "hello" == "Hello"
example_upperChar_1 = upperChar "world" == "World"
-- DEFINITION
upperChar xs = helper True xs
  where
    helper up [] = []
    helper up (x:xs) =
      (if up then toUpper x else x) : helper (isSpace x) xs

-- TESTS
prop_barIdempotent xs = upperChar xs == upperChar (upperChar xs)


--------------------------------------------------------------------------------

-- CONTRACT [TODO]
f :: Num a => a -> a
-- Dummy definition to avoid error message - please ignore
f = (1+) 
--------------------------------------------------------------------------------
