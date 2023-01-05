> import Test.QuickCheck

CONTRACT

> myOr :: [Bool] -> Bool

PURPOSE

myOr is a function that takes a list of booleans and returns a boolean 
if the list of booleans contains a True, it will returns a True otherwise False

EXAMPLES

> example_myOr_0 = myOr [] == False
> example_myOr_1 = myOr [True,True,False] == True
> example_myOr_2 = myOr [False,False,False] == False

DEFINITION

> myOr [] = False
> myOr (x:xs) = x || myOr xs

TESTS

> prop_myOr_orFunction1 xs = myOr xs == or xs
