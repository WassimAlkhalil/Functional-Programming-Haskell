module Ex01 where

import Data.Char

--------------------------------------------------------------------------------
-- CONTRACT
appendString :: String -> String -> String

-- PURPOSE
-- This function takes two strings and retuns a
-- string that is the concatenation of the two

-- EXAMPLES
-- appendString "foo" "bar" : "foobar"
-- appendString "Hello" " World" : "Hello World"

-- DEFINITION
appendString "" str2 = str2
appendString (char1 : str1) str2 = char1 : appendString str1 str2

-- TESTS
prop_appendString :: String -> String -> Bool
prop_appendString a b = appendString a b == a ++ b
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- CONTRACT
reverseString :: String -> String

-- PURPOSE
-- This function takes a string and returns a string
-- that contains the characters in reversed order

-- EXAMPLES
-- reverseString "" : ""
-- reverseString "oof" : "foo"

-- DEFINITION
reverseString = foldl (flip (:)) []

-- TESTS
prop_reverseString :: String -> Bool
prop_reverseString a = reverseString a == reverse a
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- CONTRACT
dropCharacters :: Int -> String -> String

-- PURPOSE
-- This function takes an integral number and a string, and
-- drops as many characters from the string as denoted by the integral.
-- Should the integral be negative or greater than the strings length,
-- the original string is returned.

-- EXAMPLES
-- dropCharacters 10 "" : ""
-- dropCharacters (-10) "foobar" : "foobar"
-- dropCharacters 3 "foobar" : "bar"

-- DEFINITION
dropCharacters toDrop = concatMap (\(i, c) -> if i < toDrop then [] else [c]) . zip [0..]

-- TESTS
prop_dropCharacters :: Int -> String -> Bool
prop_dropCharacters n str = dropCharacters n str == drop n str
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- CONTRACT
takeCharacters :: Int -> String -> String

-- PURPOSE
-- This function takes an integral number and a string, and
-- takes as many characters from the string as denoted by the integral.
-- Should the integral be greater than the strings length,
-- the original string is returned. Should the integral be negative,
-- the empty string is returned.

-- EXAMPLES
-- takeCharacters 10 "" : ""
-- takeCharacters (-10) "foobar" : ""
-- takeCharacters 3 "foobar" : "foo"

-- DEFINITION
takeCharacters toTake = concatMap (\(i, c) -> if i < toTake then [c] else []) . zip [0..]

-- TESTS
prop_takeCharacters :: Int -> String -> Bool
prop_takeCharacters n str = takeCharacters n str == take n str
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- DESCRIPTION
-- TODO <- "dlroW dloC"
foo :: [Char]
foo = map chr [100,108,114,111,87,32,100,108,111,67]

--------------------------------------------------------------------------------
-- DESCRIPTION
-- TODO <- "Hardware"
bar :: [Char]
bar = map chr [72,97,114,100,119,97,114,101]

--------------------------------------------------------------------------------
-- DESCRIPTION
-- TODO <- "Portobello"
baz :: [Char]
baz = map chr [80,111,114,116,111,98,101,108,108,111]

--------------------------------------------------------------------------------
-- DEFINITION
question1 :: p1 -> String -> p2 -> String
question1 foo bar baz = takeCharacters 4 bar

--------------------------------------------------------------------------------
-- DEFINITION
question2 :: String -> p1 -> p2 -> String
question2 foo bar baz = reverseString foo

--------------------------------------------------------------------------------
-- DEFINITION
question3 :: p -> String -> String -> String
question3 foo bar baz = appendString (appendString (takeCharacters 1 bar )(dropCharacters 7 bar)) (dropCharacters 7 baz)

--------------------------------------------------------------------------------
-- DEFINITION
question4 :: [Char] -> String -> String -> String
question4 foo bar baz = appendString (question3 foo bar baz)(dropCharacters 4 (reverseString foo))
