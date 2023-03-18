-- Author :: Wassim Alkhalil
----------------------------

-- Task 1
---------
data Event a = Lecture | Lab a | Exercise deriving Show  -- constructors

-- type aliases
-- has a type parameter or many type parameters
-- a type declaration can have type parameters
-- type and constructor names must always begin with an upper-case letter   

type Professor = String -- (a,a) are the constructors for the type pair
newtype Title = Title String deriving Show -- deriving clause specifies that we want the compiler to automatically generate instances of the Eq and Show classes for our Pair type
data Course a = Course Professor Title [Event a] deriving Show 

type PathToFile = String 
data EventDescription = EventDescription {    -- constructor 
     topic :: String
    ,assistants :: [String]
    } deriving Show

isLab :: Event a -> Bool
isLab (Lab _) = True 
isLab Lecture = False 
isLab Exercise  = False 

giveMeAllLabs :: Course EventDescription -> [Event EventDescription]
giveMeAllLabs (Course _ _ es) = filter isLab es

labs = giveMeAllLabs (Course "Schupp" (Title "Functional Programming") [Lecture]) 
----------------------------------------------------
-- Task 2
---------
course = Course "Prof. Brain"(Title "Esoteric Programming Languages") [Lecture, Exercise, Lab (EventDescription {topic = "Brainfuck", assistants = ["Pinky","Taxi"]}) ]
----------------------------------------------------
-- Task 3
---------
labList :: Course EventDescription -> [String]
labList (Course _ _ es) = map (\(Lab x) -> topic x ) $ filter isLab es 
----------------------------------------------------
-- Task 4
---------
data Error a = Error { 
    errorID :: Maybe Int, 
    errorDescription :: Maybe String,
    result :: Maybe a 
} deriving Show
----------------------------------------------------
-- Task 5
---------
bindE :: Error a -> (a -> Error b) -> Error b
bindE (Error id description Nothing) f = Error id description Nothing