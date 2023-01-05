> import Test.QuickCheck
  
CONTRACT

> myFindIndex :: Eq a => a -> [a] -> Int

PURPOSE

myFindIndex is a function that takes a value of type a and a list of type a and returns an Integer
if the value is in the list the function will returns the index

EXAMPLES

> example_myFindIndex_1 = myFindIndex 'a' [] == error "myFindIndex: element not found"
> example_myFindIndex_2 = myFindIndex 'a' ['a','b','c'] == 0
> example_myFindIndex_3 = myFindIndex  2  [1,2,3,4] == 1
> example_myFindIndex_4 = myFindIndex  5  [1,2,3,4] == error "myFindIndex: element not found"
> example_myFindIndex_5 = myFindIndex True [False,True,False] == 1

DEFINITON

> myFindIndex x ys = helper (zip [0..] ys)
>   where
>     helper [] = error "myFindIndex: element not found"
>     helper (z:zs) | x == snd z = fst z 
>                   | otherwise = helper zs

TESTS

 