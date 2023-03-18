import Data.List

-- Task 1
-- CONTRACT
ackermann :: Integer -> Integer -> Integer
-- PURPOSE
-- Computes the Ackermann function of two integers
-- EXAMPLES
example_akermann_0 = ackermann 0 0 == 1
example_akermann_1 = ackermann 0 1 == 2
example_akermann_2 = ackermann 1 0 == 2
example_akermann_3 = ackermann 1 1 == 3
example_akermann_4 = ackermann 2 0 == 3
-- DEFINITION
ackermann m n | m < 0 || n < 0 = error "only␣positive␣arguments␣allowed"
              | m == 0 && n >= 0 = n + 1
              | m >= 0  && n == 0 = ackermann (m - 1) 1
              | otherwise = ackermann (m - 1) (ackermann m (n - 1))

-- Task 2
-- CONTRACT
data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = add (mult n m) m

-- a) ∀x : mult (Succ Zero) x ≡ x

-- Base Case : mult (Succ Zero) Zero
-- add (mult Zero Zero) Zero
-- add Zero Zero
-- Zero

-- Inductive Case : mult (Succ Zero) (Succ x)
-- add (mult (Zero) (Succ Zero)) (Succ x)
-- add (Zero) (Succ x)
-- Succ x


-- b) ∀x : mult x (Succ Zero) ≡ x

-- Base Case : 
-- mult Zero (Succ Zero)
-- Zero

-- Inductive Case : 
-- mult (Succ x) (Succ Zero)
-- add (mult x (Succ Zero)) (Succ Zero)
-- add x (Succ Zero) 
-- Succ x 

-- Task 3
-- :t ['f', 'p']
-- ['f', 'p'] :: [Char]

-- :t ([23,42], ())
-- ([23,42], ()) :: Num a => ([Int],())

-- :t ([])
-- ([]) :: [a]

-- :t [foldr (:) [] "foldr"]
-- [foldr (:) [] "foldr"] :: [[Char]]

-- Task 4
isSubsetOf :: Ord a => [a] -> [a] -> Bool
xs `isSubsetOf` ys = sort (nub xs) `helper` sort (nub ys)
                        where
                            [] `helper` _ = True
                            _ `helper` [] = False
                            (x:xs) `helper` (y:ys) | x == y = xs `isSubsetOf` ys
                                                   | x > y = (x:xs) `isSubsetOf` ys
                                                   | otherwise = False
-- isSubsetOf is a function that takes two lists and returns a boolean value
-- the function terminates for finite lists and returns True if the first list is a subset of the second list