import Test.QuickCheck

-- Task 1

-- a

data Nat = Zero | Succ Nat deriving Show

-- Nat is a type with two constructors. The constructor Zero has no argument.
-- but the constructor Succ takes an argument of type Nat.
-- Nat is a recursive type since the constructor Succ takes an argument of type Nat.
-- only data declarations permit recursive.

-------------------------------------------------------
-- b

-- CONTRACT
intToNat :: Int -> Nat

-- PURPOSE
-- Takes an integer and returns a natural number

-- EXAMPLE
-- intToNat 0 = Zero
-- intToNat 3 = Succ(Succ(Succ Zero))

-- DEFINITION
intToNat 0 = Zero
intToNat n | n < 0 = error "Invalid Number"
           | otherwise = Succ (intToNat (n-1))

{-

intToNat 4 = Succ(intToNat(3))
           = Succ(Succ(intToNat 2))
           = Succ(Succ(Succ(intToNat 1)))
           = Succ(Succ(Succ(Succ(intToNat 0))))
           = Succ(Succ(Succ(Succ Zero)))
-}
-------------------------------------------------------
-- CONTRACT
natToInt :: Nat -> Int

-- PURPOSE
-- Takes an natural number and returns an intger 

-- EXAMPLE
-- natToInt Zero = 0
-- natToInt (Succ (Succ (Succ Zero))) = 1

-- DEFINTION
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

{-
natToInt (Succ (Succ (Succ Zero))) 
natToInt (Succ (Succ 0))
natToInt (Succ )
-}
-------------------------------------------------------
-- CONTRACT
add :: Nat -> Nat -> Nat

-- PURPOSE
-- Takes two natural numbers and returns their sum

-- EXAMPLE
add Zero ( Succ Zero ) = Succ Zero

-- DEFINITION
add Zero n = n
add (Succ m) n = Succ (add m n)
-------------------------------------------------------

-- Task 2

-- CONTRACT
mult :: Nat -> Nat -> Nat

-- PURPOSE
-- The function mult that takes two Nats and multiplies them 

-- EXAMPLE
-- mult Zero Zero = 0
-- mult (Succ(Succ(Succ Zero))) (Succ Zero) = Succ (Succ Zero)
-- mult Zero (Succ Zero) = Zero

-- DEFINITION
mult Zero _ = Zero
mult (Succ n) m = add n (mult m n)
{-
mult (Succ (Succ Zero)) (Succ Zero)
add (Succ Zero) mult (Succ Zero) (Succ Zero)
add (Succ Zero) add (Succ Zero) mult (Zero) (Succ Zero)
-}
-- TESTS
prop_mult :: Int -> Int -> Bool
prop_mult m n = natToInt (mult (intToNat m) (intToNat n)) == m * n
                                               
-------------------------------------------------------

-- Task 3

data BDD = TrueSink Bool| FalseSink Bool | Node String BDD BDD deriving Show

-- Task 4
{-
x1 x2 f(x1,x2)
0  0   0
0  1   1
1  0   0
1  1   1
-}
-- BDD = Node "x1" (Node "x2" (FalseSink False) (TrueSink True)) (Node "x2" (FalseSink False) (TrueSink True))
-------------------------------------------------------


