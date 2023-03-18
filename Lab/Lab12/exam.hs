import Data.Char (isUpper)
import Data.List (nub)

import Test.QuickCheck

-- Types and Type Classes

-- a) 
{-
:t True 
True :: Bool

:t ("False", not False)
("False", not False) :: (String, Bool)

:t [take, drop]
[take, drop] :: [Int -> [a] -> [a]]

:t (tail [1,2,3], tail (show [1,2,3]))
(tail [1,2,3], tail (show [1,2,3])) :: Num a => ([a] -> [Char])
-}

-- b)
-- CONTRACT
prntBtwnNth :: (Integral a) => a -> [b] -> [b] -> [b]

-- DEFINITION
prntBtwnNth n del str = let ys = zip str [0..] in foldr (\(z,i) acc -> if (i `mod` n) /= 0 || i == 0
                                                                            then z:acc 
                                                                                else del ++ z:acc) [] ys
-- CONTRACT
anyCloseTo :: (Ord a, Num a) => a -> a -> [a] -> Bool

-- DEFINITION
anyCloseTo v d = any ((<d).abs.((-)v))


-- List Comprehension
-- a)
-- not compiling.
-- compiling, [(3,2),(3,3),(3,4)]
-- compiling, ["oo", "ar", "az"]

-- b)
-- CONTRACT
getIndicesOfUpper :: String -> [Int]

-- DEFINITION
getIndicesOfUpper str = map snd $ filter (\(c,_) -> isUpper c) $ zip str [0..]
-- other solution
getIndicesOfUpper' str = foldr(\x acc -> [i | (c,i) <- zip str [0..], isUpper c]) [] str

-- other solution
{-
getIndicesOfUpper :: String -> [Int]
getIndicesOfUpper str = indexFind (zip [0..] str)
            where
                indexFind []    = []
                indexFind ((x,y):xs) 
                    | isUpper y   = x : indexFind xs
                    | otherwise   = indexFind xs
-}


-- NOTE: map and filter programming in haskell are very importan.

-- Pattern Matching 
-- a)
-- 1) Does Match: a = "oo"
-- 2) Does Not Match
-- 3) Does Match: a = [2] , b = [3,4] , xs = [[5,6]]
-- 4) Does Match: n = "2.058P2" , c = 15

-- b)
data StudyProgram = CompEng | CompScience | TechnoMath | BioProcEng
                  | ElectricEng | EnergyEng | Logistics | MechEng
                  | Mechatronics | NavalArch | ProcEng deriving (Eq)

studyDetails :: (StudyProgram, Int) -> String

studyDetails (_,1) = "Freshman" 
studyDetails (CompEng, _) = "Higher␣semester␣informatics"
studyDetails (CompScience, _) = "Higher␣semester␣informatics"
studyDetails (_,_) = "Other␣higher␣semester"

-- Recursion

-- a) 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Base Cases Are: 
-- zipWith' f [] _ = []
-- zipWith' f _ [] = []

-- Recursive Cases:
-- zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Under which conditions does the function terminates and why? Consider both finite and infinite inputs.
{-
When the list is finite, the function terminates.
This is because in the recursive cases we take out one element out one element from each of the input lists, so they will eventually be empty. 
Once, the function reachs the base case, it terminates

For infinite lists, the function does not terminate. 
-}

-- b)
-- CONTRACT
replicate' :: Int -> a -> [a]

-- EXAMPLE
example_replicate'_0 = replicate' 0 'a' == ""
example_replicate'_1 = replicate' 3 'a' == "aaa"

-- DEFINITION
replicate' 0 chr = []
replicate' n chr = if n < 0 
                        then error "choose a number greater than zero" 
                                else chr : replicate' (pred n) chr

-- Higher-Order Functions
-- a) 
-- CONTRACT
func :: a -> b -> (a -> b -> c -> d) -> (b -> c) -> (b -> d -> e) -> e

-- DEFINITION
func a b f g h = h b (f a b (g b))

-- b) 
{-
data Item = Apple | Carrot | Chocolate deriving (Eq)

advice items = 
    let (hPr,uPr) = 
        foldr 
            (\(it,num,pr) (hAcc, uAcc) -> 
                case it of
                    Apple -> (hAcc+(num*pr), uAcc)
                    Carrot -> (hAcc+(num*pr), uAcc)
                    Chocolate -> (hAcc, uAcc+(num*pr))
        ) 
        (0,0) 
        items 
    in case hPr > uPr of
      True -> "Well␣done."
      False -> "Buy␣more␣healthy␣food!
-}

-- CONTRACT
-- advise :: (Num a , Ord a) => [(Item, a, a)] -> String

-- PURPOSE 
-- Takes a list of tuples as input, where the first tuple elment is an Item.
-- the second is the amount we purchased of that item, and the third is the price of the item. 
-- then if we spend more on healty food it outputs "well done", otherwise it tell us to buy more healty food.

-- EXAMPLE
-- example_advise_0 = advise [] == "Buy more healthy food!"
-- example_advise_2 = advise [(Apple,1,1)] == "Well done"

-- c)
-- later

-- User-defined Types
-- a)
type TimeEntry = (Int, Int) -- (hour, minutes)
type DateEntry = (Int, Int, Int) -- (year, month, day)
type Year = Int
type ECTS = Int
type Professor = String
type Topic = String
data Term = WinterTerm | SummerTerm deriving Show
data SemesterData = SemesterData Year Term [Course] deriving Show
data Course = Course {
    lecturer :: Professor,
    topic :: Topic,
    ects :: ECTS,
    lectureSlots :: [LectureSlot]
} deriving Show
data LectureSlot = LectureSlot {
    date :: DateEntry,
    startTime :: TimeEntry,
    endTime :: TimeEntry,
    slotTopic :: Topic
} deriving Show
-- i) 
semesterData = SemesterData 2020 WinterTerm [Course "Prof. Schupp" "FP" 6 [LectureSlot (2020,01,29)(9,45)(11,15)"Revision"]] 

-- ii) 
-- Course { lecturer="Marrone", topic="ML", ects=6 } : Not Compiling 
-- Term WinterTerm : Not Compiling
-- SemesterData 2025 SummerTerm [] : Compiling

-- b)

type MountLetter = String 
type Name = String
type Data = String 

data StorageDevice = StorageDevice [Partition]
data Partition = Partition MountLetter FielSystem
data FielSystem = FileSystem [FileSystemTreeItem]
data FileSystemTreeItem = Directory Name [FileSystemTreeItem] | File Name Data 

-- Evaluation 
-- a) 
-- i) take 2 $ drop 2 $ cycle [1]
-- yes, because of the lazy take which takes just 2 elments from the infinite list created by cycle.

-- ii) zip [0..] [1,2,3]
-- yes, because zip is lazy and terminates as soon as one of the lists is accounted for, so after 3 zips it stops.

-- iii) minimum [1,2..]
-- no, because minimum is not lazy and needs to check all elements.

-- b)
-- i) True 
-- ii) (10,15)
-- iii) [2,3,4]

-- c)
-- checkPrices3 can handle infinite lists because it terminates as soon as the sum of prices of above the cap value. 
-- checkPrices1 has no base case to stop the recursion early, so it needs to go through the entire list 
-- the same applies to checkprices2, just with map instead of recursion.


-- Reasoning and Testing
-- a)
{-
-- CONTRACT
prob_nubRet_test :: [Int] -> Bool 

-- i) The number of elements in nubRet is smaller or equal to the number of elements in xs
prop_nubRet_test xs = nubRet xs == length(nubRet xs) <= length xs

-- ii) ii) nubRet contains only unique elements (i.e., no duplicates)
prop_nubRet_test (x:xs) = nubRet (x:xs) == if x `elem` xs
                                                then nub' xs
                                                    else x : nub' xs
-}

-- b)
data Nat = Zero | Succ Nat

mod' :: Nat -> Nat -> Nat
mod' m n = modHelper n Zero m n

modHelper :: Nat -> Nat -> Nat -> Nat -> Nat
modHelper Zero _ a b = mod' a b
modHelper _ r Zero b = r
modHelper (Succ c) r (Succ a) b = modHelper c (Succ r) a b

-- ∀ y: mod' y (S Z) == Zero

-- Base Case : 
-- mod' y (Succ Zero)
-- mod' Zero (Succ Zero) 
-- modHelper (Succ Zero) Zero Zero (Succ Zero)
-- Zero

-- Inductive Case: 
-- mod' (Succ y) (Succ Zero) 
-- modHelper (Succ Zero) Zero (Succ y) (Succ Zero) 
-- modHelper Zero (Succ Zero) y (Succ Zero) 
-- mod' y (Succ Zero) --  here we reached the Base Case  mod' y (Succ Zero) == Zero
-- Zero 


