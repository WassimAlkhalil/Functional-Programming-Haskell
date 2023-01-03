import Control.Applicative
import Data.Char
import System.IO           (hSetEcho, stdin)

{-

Programming in Haskell (1st edition) by Graham Hutton <http://www.cs.nott.ac.uk/~pszgmh/pih.html>

Contents:

1. Functions
2. First steps
3. Types and classes
4. Defining functions
5. List comprehensions
6. Recursive functions
7. Higher-order functions
8. Functional parsers
9. Interactive programming
10. Declaring types and classes
11. The countdown problem
12. Lazy evaluation
13. Reasoning about programs

-}

--------------------------------------------------------------------------------
-- 1. Introduction
--------------------------------------------------------------------------------

double x = x + x

sum' []     = 0
sum' (x:xs) = x + sum xs

qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]

-- 1.7 (p.10) ------------------------------------------------------------------

rqsort []     = []
rqsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

product' []     = 1
product' (x:xs) = x * product' xs

--------------------------------------------------------------------------------
-- 2. First steps
--------------------------------------------------------------------------------

{-

-- arithmetic
+, -, *, `div`, ^

-- list operations
head, tail, !!, take, drop, length, sum, product, ++, reverse

-- error
1 `div` 0
head []

-- function

==========  =========
Math        Haskell
==========  =========
f(x)        f x
f(x,y)      f x y
f(g(x))     f (g x)
f(x,g(y))   f x (g y)
f(x)g(y)    f x * g y
==========  =========

-- interpreter
:load <file>
:reload

quandruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns
-- average ns = div (sum ns) (length ns)

-- reserved words
case   class    data   default deriving do
else   if       import in      infix    infixl
infixr instance let    module  newtype  of
then   type     where

-- layout
a = b + c
    where
      b = 1
      c = 2
a = b + c
    where
      { b = 1;
        c = 2 }

-}

-- 2.6 (p.18) ------------------------------------------------------------------

last1 xs = xs !! (length xs - 1)
last2 xs = head (reverse xs)
last3 xs = head (drop (length xs - 1) xs)
last4 [x]    = x
last4 (x:xs) = last4 xs

init1 xs = take (length xs - 1) xs
init2 xs = reverse (drop 1 (reverse xs))
init3 [a]    = []
init3 (x:xs) = x : init3 xs

--------------------------------------------------------------------------------
-- 3. Types and classes
--------------------------------------------------------------------------------
{-

False :: Bool
True :: Bool

> :type not
not :: Bool -> Bool
> :type not
not False :: Bool
> :type not 3
(Error msg)

-- basic types
Bool, Char, String, Int, Integer, Float

-- list
[Bool], [Char], [[Char]], ...

-- tuple
(False, True) :: (Bool, Bool)
(["Yes", "No"], (True, 'a')) :: ([String], (Bool, Char))

-- function
isDigit :: Char -> Bool

add :: (Int,Int) -> Int
add (x,y) = x + y

-- currying
add' :: Int -> Int -> Int
add' x y = x + y

-- polymorphism
length :: [a] -> Int
fst, head, take, zip, id

-- class constraints
> 1 + 2
3
> 1.1 + 2.2
3.3
> :type (+)
(+) :: Num a => a -> a -> a

-- basic classes
Eq
  (==) (/=)         :: a -> a -> Bool
  Bool, Char, String, Int, Integer, Float

Ord
  (<) (<=) (>) (>=) :: a -> a -> Bool
  min, max          :: a -> a -> a
  Bool, Char, String, Int, Integer, Float

Show
  show              :: a -> String
  Bool, Char, String, Int, Integer, Float

Read
  read              :: String -> a
  Bool, Char, String, Int, Integer, Float

Num
  (+) (-) (*)           :: a -> a -> a
  negate, abs, signum   :: a -> a
  Int, Integer, Float

Integral
  (/)               :: a -> a -> a
  recip             :: a -> a
  Float

-- 3.11 (p.33) -----------------------------------------------------------------

['a', 'b', 'c'] :: [Char]
('a', 'b', 'c') :: (Char)
[(False, 'o'),(True, '1')] :: [(Bool, Char)]
([False, True], ['0', '1']) :: ([Bool], [Char])

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome Eq => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-}

--------------------------------------------------------------------------------
-- 4. Defining functions
--------------------------------------------------------------------------------

isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 2

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1 / n

-- conditional
abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n

abs2 n | n > 0     = n
       | otherwise = -n

signum1 :: Int -> Int
signum1 n = if n < 0 then -1 else
              if n == 0 then 0 else 1

signum2 n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

-- pattern match
not' :: Bool -> Bool
not' False = True
not' True  = False

and1 :: Bool -> Bool -> Bool
True  `and1` True  = True
True  `and1` False = True
False `and1` True  = True
False `and1` False = True

-- wildcard
True `and2` True = True
_    `and2` _    = False

True  `and3` b = b
False `and3` _ = False

b `and4` c | b == c    = b
           | otherwise = False

-- tuple pattern
fst' :: (a, b) -> a
fst' (x, _) = x
snd' :: (a, b) -> b
snd' (_, y) = y

-- list pattern
null' :: [a] -> Bool
null' [] = True

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

-- lambda function
{-

> (\x -> x + x) 2
4

add x y = x + y
add = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

const :: a -> (b -> a)
const x = \_ -> x

odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = x * 2 + 1
odds' n = map (\x -> x * 2 + 1) [0..n-1]

-- section
(+) = \x -> (\y -> x + y)
(x +) = \y -> x + y
(+ y) = \x -> x + y

and :: [Bool] -> Bool
and = foldr (&&) True

-}

-- 4.8 (p.43) ------------------------------------------------------------------

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

-- safe tail

-- a. conditional
safetail1 xs = if null xs == True then [] else drop 1 xs
-- b. guard
safetail2 xs | null xs   = []
             | otherwise = drop 1 xs
-- c. pattern match
safetail3 []     = []
safetail3 (x:xs) = xs

-- or (||)

True  `or1` True  = True
True  `or1` False = True
False `or1` True  = True
False `or1` False = False

False `or2` False = False
_     `or2` _     = True

False `or3` b = b
True  `or3` _ = True

b `or4` c  | b == c    = b
           | otherwise = True

-- True && True = True
-- _    || _    = False
a `and5` b = if a == True then if b == True then True
                                            else False
                          else False

-- True && b = b
-- False || _ = False
a `and6` b = if a == True then b else False

-- mult x y z = x * y * z
mult = \x -> (\y -> (\z -> x * y * z))

--------------------------------------------------------------------------------
-- 5. List comprehensions
--------------------------------------------------------------------------------
{-

[x^2 | x <- [1..5]]

[(x,y) | x <- [1,2,4], y <- [4,5]]
[(x,y) | y <- [4,5], x <- [1,2,4]]

[(x,y) | x <- [1..3], y <- [x..3]]

-}

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x,_) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

evens :: Integral a => [a] -> [a]
evens xs = [x | x <- xs, even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

-- "abc" :: String is ['a', 'b', 'c'] :: [Char]
{-
> "abcde" !! 2
'c'
> take 3 "abcde"
"abc"
> length "abcde"
5
> zip "abc" [1,2,3,4]
[('a', 1), ('b', 2), ('c', 3)]

-}

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- 5.5. Caesar Chipher

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

-- Chi-squared test: $\sum_{i=0}^{n-1} \frac{(os_i - es_i)^2}{es_i}$

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2) / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [0..25]]
             table' = freqs xs

-- 5.7 (p.55) ------------------------------------------------------------------

-- sum [x^2 | x <- [1..100]]

replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x<-[1..n], y<-[1..n], z<-[1..n],
                       x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], let fs = factors x,
                  sum (init fs) == last fs]

-- concat [ [(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

positions' x xs = find x (zip xs [0..(length xs - 1)])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Upper letter compatible Caesar chipher

let2int' :: Char -> Int
let2int' c | isLower c = ord c - ord 'a'
           | isUpper c = ord c - ord 'A'

int2letl :: Int -> Char
int2letl n = chr (ord 'a' + n)

int2letu :: Int -> Char
int2letu n = chr (ord 'A' + n)

shift' :: Int -> Char -> Char
shift' n c | isLower c = int2letl n'
           | isUpper c = int2letu n'
           | otherwise = c
           where n' = ((let2int' c + n) `mod` 26)

encode' :: Int -> String -> String
encode' n xs = [shift' n x | x <- xs]

freqs' :: String -> [Float]
freqs' xs = [percent ((count l xs) + (count u xs)) len | (l, u) <- ts]
            where
              ts = zip ['a'..'z'] ['A'..'Z']
              len = length xs

crack' :: String -> String
crack' xs = encode' (-factor) xs
            where
              factor = head (positions (minimum chitab) chitab)
              chitab = [chisqr (rotate n table') table | n <- [0..25]]
              table' = freqs' xs

--------------------------------------------------------------------------------
-- 6. Recursive functions
--------------------------------------------------------------------------------

factorial :: Int -> Int
factorial n = product [1..n]

factorial' 0 = 1
factorial' n = n * factorial (n - 1)

-- (*)
mul :: Int -> Int -> Int
m `mul` 0 = 0
m `mul` n = m + (m * (n - 1))

length1 :: [a] -> Int
length1 []     = 0
length1 (_:xs) = 1 + length1 xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse xs ++ [x]

-- (++)
add' :: [a] -> [a] -> [a]
[] `add'` ys     = ys
(x:xs) `add'` ys = x:(xs `add'` ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y:insert x ys

-- insertion sort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

even1 :: Int -> Bool
even1 0 = True
even1 n = odd (n - 1)

odd1 :: Int -> Bool
odd1 0 = False
odd1 n = even (n - 1)

evens1 :: [a] -> [a]
evens1 []     = []
evens1 (x:xs) = x : odds1 xs

odds1 :: [a] -> [a]
odds1 []     = []
odds1 (_:xs) = evens1 xs

-- 6.7 (p.71) ------------------------------------------------------------------

-- (^)
pow :: (Num a, Eq a, Integral b) => a -> b -> a
0 `pow` _ = 0
_ `pow` 0 = 1
n `pow` e = n * (n `pow` (e - 1))

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat1 :: [[a]] -> [a]
concat1 []       = []
concat1 (xs:xss) = xs ++ concat1 xss

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n - 1) x

-- (!!)
(!!!) :: [a] -> Int -> a
(x:_) !!! 0  = x
(_:xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x    = True
               | otherwise = elem' a xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs = merge lhs rhs
           where
             lhs = msort (fst hs)
             rhs = msort (snd hs)
             hs = halve xs

sum1 :: Num a => [a] -> a
sum1 []     = 0
sum1 (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last4 xs

--------------------------------------------------------------------------------
-- 7. Higher-order functions
--------------------------------------------------------------------------------

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 f []     = []
map2 f (x:xs) = f x : map f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p xs = [x | x <- xs, p x]

filter2 p [] = []
filter2 p (x:xs) | p x       = x : filter2 p xs
                 | otherwise = filter2 p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

{-
> all even [2,4,6,8]
True
> any odd [2,4,6,8]
False

> takeWhile isLower "abc def"
"abc"
> dropWhile isLower "abc def"
" def"

-- foldr
sum     = foldr (+) 0
product = foldr (*) 1
or      = foldr (||) False
and     = foldr (&&) True

-- foldl
sum     = foldl (+) 0
product = foldl (*) 1
or      = foldl (||) False
and     = foldl (&&) True

-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs

length2 = foldr' (\_ n -> 1 + n) 0
length3 = foldl' (\n _ -> n + 1) 0

snoc x xs = xs ++ [x] -- cons reversed
reverse1 []     = []
reverse1 (x:xs) = snoc x (reverse1 xs)
reverse2 = foldr' snoc []
reverse3 = foldl' (\xs x -> x : xs) []

-- (.)
circ :: (b -> c) -> (a -> b) -> (a -> c)
f `circ` g = \x -> f (g x)

odd2 = not . even   -- same as: odd2 n = not (even n)
twice f x = f . f   -- same as: twice f x = f (f x)
sumsqreven' = sum . map (^2) . filter even

id' :: a -> a
id' = \x -> x
-- id . f = f and f . id = f

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- string conversion

type Bit = Int
bin2int :: [Bit] -> Int
bin2int' bits = sum [w * b | (w, b) <- zip weights bits]
                where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode1 :: String -> [Bit]
encode1 = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode1

channel :: [Bit] -> [Bit]
channel = id -- assume no error

-- 7.7 (p.87) ------------------------------------------------------------------

-- 1 -------------------------------------------------------
-- [f x | x <- xs, p x]
-- map f (filter p xs)

-- 2 -------------------------------------------------------
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (&&) True (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (||) False (map p xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x : dropWhile' p xs

-- 3 -------------------------------------------------------
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr f []
            where f x xs | p x       = x:xs
                         | otherwise = xs

-- 4 -------------------------------------------------------
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

-- 5 -------------------------------------------------------
{-
sumsqreven = compose [sum, map (^2), filter even]
  is wrong because:
    sum         :: Num a => [a] -> a
    map, filter ::          [a] -> [a]
-}

-- 6 -------------------------------------------------------
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- 7 -------------------------------------------------------
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8' = unfold null (take 8) (drop 8)

map3 :: (a -> b) -> [a] -> [b]
map3 f = unfold null (f . head) (drop 1)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

-- 8 -------------------------------------------------------
-- parity check

encode2 :: String -> [Bit]
encode2 = concat . map (make9 . int2bin . ord)

decode1 :: [Bit] -> String
decode1 = map (chr . bin2int) . chop9

make9 :: [Bit] -> [Bit]
make9 = addParity . take 8 . (++ repeat 0)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (checkParity . take 9) (drop 9)

addParity :: [Bit] -> [Bit]
addParity bits | odd (sum bits) = bits ++ [1]
               | otherwise      = bits ++ [0]

checkParity :: [Bit] -> [Bit]
checkParity bits | odd si && l == 1 || even si && l == 0 = i
                 | otherwise = error "Parity check failed."
                 where
                   si = sum i
                   i = init bits
                   l = last bits

-- 9 -------------------------------------------------------
transmit' :: String -> String
transmit' = decode1 . channel' . encode2

channel' :: [Bit] -> [Bit]
channel' = tail

--------------------------------------------------------------------------------
-- 8. Functional parsers
--------------------------------------------------------------------------------

-- type Parser = String -> Tree
-- type Parser = String -> (Tree, String)
-- type Parser = String -> [(Tree, String)]

type Parser a = String -> [(a, String)]

ret :: a -> Parser a
ret v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp
-- > parse (ret 1) "abc"
-- [(1, "abc")]
-- > parse failure "abc"
-- []
-- > parse item ""
-- []
-- > parse item "abc"
-- [('a', "bc")]

-- (>>=) bind
-- Parser a -> Parser b -> Parser (a, b)
(=>>) :: Parser a -> (a -> Parser b) -> Parser b
p =>> f = \inp -> case parse p inp of
                    []         -> []
                    [(v, out)] -> parse (f v) out
{-
p1 >>= \v1 ->
p2 >>= \v2 ->
...
pn >>= \vn ->
return (f v1 v2 ... vn)

    |
    v

do v1 <- p1
   v2 <- p2
   ...
   vn <- pn
   return (f v1 v2 ... vn)
-}
-- bind
p :: Parser (Char, Char)
p = item =>> \x ->
    item =>> \_ ->
    item =>> \y ->
    ret (x, y)
-- > parse p "abcdef"
-- [(('a', 'c'), "def")]
-- > parse p "ab"
-- []

-- selection
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    []         -> parse q inp
                    [(v, out)] -> [(v, out)]
-- > parse (item +++ ret 'd') "abc"
-- [('a', "bc")]
-- > parse (item +++ ret 'd') ""
-- [('d', "")]
-- > parse (failure +++ failure) "abc"
-- []

-- single characte parsers
sat :: (Char -> Bool) -> Parser Char
sat p = item =>> \x ->
        if p x then ret x
               else failure

digit :: Parser Char
digit = sat isDigit
-- > parse digit "123"
-- [('1', "23")]

lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)
-- > parse (char 'a') "abc"
-- [('a', "bc")]

string :: String -> Parser String
string []     = ret []
string (x:xs) = char x =>> \_ ->
                string xs =>> \_ ->
                ret (x:xs)
-- > parse (string "abc") "abcdef"
-- [("abc", "def")]

-- string parsers
many' :: Parser a -> Parser [a]
many' p = many1 p +++ ret []
many1 :: Parser a -> Parser [a]
many1 p = p =>> \v ->
          many' p =>> \vs ->
          ret (v:vs)
-- > parse (many' digit) "123abc"
-- [("123", "abc")]
-- > parse (many' digit) "abcdef"
-- [("", "abcdef")]
-- > parse (many1 digit) "abcdef"
-- []

ident :: Parser String
ident = lower =>> \x ->
        many' alphanum =>> \xs ->
        ret (x:xs)
-- > parse ident "abc def"
-- [("abc", " def")]

nat :: Parser Int
nat = many1 digit =>> \xs ->
      ret (read xs)
-- > parse nat "123 abc"
-- [(123, " abc")]

space :: Parser ()
space = many' (sat isSpace) =>> \_ ->
        ret ()
-- > parse space "  abc"
-- [((), "abc")]

token :: Parser a -> Parser a
token p = space =>> \_ ->
          p =>> \v ->
          space =>> \_ ->
          ret v
-- > parse (token nat) "  123  abc"
-- [(123, "abc")]

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

parseList :: Parser [Int] -- parse non-empty list of natural numbers
parseList = symbol "[" =>> \_ ->
            natural =>> \n ->
            many' (symbol "," =>> \_ -> natural) =>> \ns ->
            symbol "]" =>> \_ ->
            ret (n:ns)
-- > parse parseList " [1, 2, 3] "
-- [([1,2,3], "")]
-- > parse parseList " [1, 2,] "
-- []

-- simple formula BNF (assume right assoc)
--   expr ::= term ('+' expr | e)
--   term ::= factor ('*' term | e)
--   factor ::= '(' expr ')' | nat
--   nat ::= '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
--   (e = empty)

expr :: Parser Int
expr = term =>> \t ->
         (symbol "+" =>> \_ ->
           expr =>> \e ->
             ret (t + e))
         +++ ret t

term :: Parser Int
term = factor =>> \f ->
         (symbol "*" =>> \_ ->
           term =>> \t ->
             ret (f * t))
         +++ ret f

factor :: Parser Int
factor = (symbol "(" =>> \_ ->
           expr =>> \e ->
             symbol ")" =>> \_ ->
               ret e)
         +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
-- > eval "2*3+4"
-- 10
-- > eval "2 * (3 + 4)"
-- 14
-- > eval "2*3-4"
-- Error: unused input -4
-- > eval "-1"
-- Error: invalid input

-- 8.10 (p.103) ----------------------------------------------------------------

-- 1 -------------------------------------------------------
int :: Parser Int
int = (symbol "-" =>> \_ ->
        natural =>> \i ->
        ret (-i))
      +++ natural

-- 2 -------------------------------------------------------
comment :: Parser ()
comment = symbol "--" =>> \_ ->
          many' (sat (/= '\n')) =>> \_ ->
          char '\n' =>> \_ ->
          ret ()

-- 6 -------------------------------------------------------
-- expr ::= term ('+' expr | '-' expr | e)
-- term ::= factor ('*' term | '/' term | e)

expr' :: Parser Int
expr' = term' =>> \t ->
              (symbol "+" =>> \_ ->
                expr' =>> \e ->
                  ret (t + e))
          +++ (symbol "-" =>> \_ ->
                expr' =>> \e ->
                  ret (t - e))
          +++ ret t

term' :: Parser Int
term' = factor' =>> \f ->
              (symbol "*" =>> \_ ->
                term' =>> \t ->
                  ret (f * t))
          +++ (symbol "/" =>> \_ ->
                term' =>> \t ->
                  ret (f `div` t))
          +++ ret f

factor' :: Parser Int
factor' = (symbol "(" =>> \_ ->
            expr' =>> \e ->
              symbol ")" =>> \_ ->
                ret e)
          +++ int

-- > expr' "2 - 1"
-- [(1, "")]
-- > expr' "4 / 2"
-- [(2, "")]

-- term1 ::= term2 ('*' term1 | '/' term1 | e)
-- term2 ::= factor ('^' term2 | e)

-- 7 -------------------------------------------------------
term1 :: Parser Int
term1 = term2 =>> \t2 ->
               (symbol "*" =>> \_ ->
                 term1 =>> \t1 ->
                   ret (t2 * t1))
           +++ (symbol "/" =>> \_ ->
                 term1 =>> \t1 ->
                   ret (t2 `div` t1))
           +++ ret t2

term2 :: Parser Int
term2 = factor =>> \f ->
             (symbol "^" =>> \_ ->
               term2 =>> \t2 ->
                 ret (f ^ t2))
          +++ ret f
-- > term1 "2^3 * 4"
-- [(32, "")]

-- 8 -------------------------------------------------------
-- simple formula BNF (left assoc)

-- a. expr ::= expr '-' nat | nat
--    nat ::= '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'

-- b.
expr1 :: Parser Int
expr1 = expr1 =>> \e ->
           (symbol "-" =>> \_ ->
             natural =>> \n ->
               ret (e - n))
        +++ natural

-- c. infinite loop

-- d.
expr2 :: Parser Int
expr2 = natural =>> \n ->
             (many' (symbol "-" =>> \_ -> natural) =>> \ns ->
               ret (foldl (-) n ns))
          +++ natural

--------------------------------------------------------------------------------
-- 9. Interactive programs
--------------------------------------------------------------------------------
{-

type IO = World -> World
type IO a = World -> (a, World)
e.g.    Char -> IO Int
     is Char -> World -> (Int, World)

getChar :: IO Char
getChar = ... (builtin)

putChar :: Char -> IO ()
putChar c = ... (builtin)
-- > putChar 'a'
-- a

return :: a -> IO a
return v = \world -> (v, world)

(>>=) :: IO a -> (a -> IO b) -> IO b
f >>= g = \world -> case f world of
                      (v, world') -> g v world'
-}

echo = do c <- getChar
          putChar '\n'
          putChar c
          putChar '\n'

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

putStr1 xs = seqn [putChar x | x <- xs]

-- http://www.cs.nott.ac.uk/~pszgmh/errata.html
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- calclator

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
             standard = "qcd=123+456-789*0()/"
             extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display xs = do writeat (3, 2) "             " -- clear display
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval1 xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case parse expr' xs of
             [(n, [])] -> calc (show n)
             _         -> do beep
                             calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

-- game of life
-- * living cell: if 2 or 3 neighbors -> survive (live)
--                otherwise           -> dead
-- * dead cell: if 3 neighbors -> birth
--              otherwise      -> keep (dead)

width :: Int
width = 100
height :: Int
height = 60

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "o" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1),
                           (x-1, y  ),           (x+1, y  ),
                           (x-1, y+1), (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 100000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

block :: Board
block = [(1,1), (1,2), (2,1), (2,2)]
tub :: Board
tub = [(2,1), (1,2), (2,3), (3,2)]
boat :: Board
boat = [(1,1), (1,2), (2,3), (3,2), (3,3)]
pentadecathlon :: Board
pentadecathlon = [(1,2), (2,1), (2,2), (2,3), (7,1), (7,2), (7,3), (8,2)]
clock :: Board
clock = [(2,1), (3,2), (4,2), (1,3), (2,3), (3,4)]

-- 9.9 (p.117) -----------------------------------------------------------------

-- 1 -------------------------------------------------------
readLine :: IO String
readLine = readLine' []
readLine' :: String -> IO String
readLine' xs = do
  x <- getCh
  case x of
     '\n'   -> do putChar '\n'
                  return (reverse xs)
     '\DEL' -> do putStr "\ESC[1D \ESC[1D"
                  readLine' (tail xs)
     _      -> do putChar x
                  readLine' (x:xs)

-- 2 -------------------------------------------------------
eval1 :: String -> IO ()
eval1 xs = case parse expr' xs of
             [(n, [])] -> calc (show n)
             [(_, r)]  -> do showError r
                             calc xs
showError :: String -> IO ()
showError xs = writeat (1, 14) ("Parse error at: " ++ xs)

-- 3 -------------------------------------------------------
life' :: Board -> IO ()
life' b = do cls
             life1 [] b
life1 :: Board -> Board -> IO ()
life1 pre b = do showcells' pre b
                 wait 100000
                 life1 b (nextgen b)

showcells' :: Board -> Board -> IO ()
showcells' pre b = do seqn [writeat p "o" | p <- b, not (p `elem` pre)]
                      seqn [writeat p " " | p <- pre, not (p `elem` b)]

-- 4 -------------------------------------------------------
makeboard :: IO ()
makeboard = do cls
               seqn [writeat (x, y) "." | x <- [0..width], y <- [0..height]]
               b <- editboard [] (1,1)
               life' b

editboard :: Board -> Pos -> IO Board
editboard b (x,y) = do
  goto (x,y)
  inp <- getCh
  case inp of
    'q' -> return b
    'j' -> f1 (x, y+1)
    'k' -> f1 (x, y-1)
    'l' -> f1 (x+1, y)
    'h' -> f1 (x-1, y)
    'J' -> f2 (x, y+1)
    'K' -> f2 (x, y-1)
    'L' -> f2 (x+1, y)
    'H' -> f2 (x-1, y)
    _   -> do writeat (x,y) "o"
              editboard ((x,y):b) (x,y)
  where
    f1 p = editboard b (wrap p)
    f2 p = do let p' = wrap p
              writeat p' "o"
              editboard (p':b) p'

-- 5 -------------------------------------------------------
-- TODO: Use Graphics.Gloss to make GUI version of calclator and Game of Life.

-- 6 -------------------------------------------------------
-- Nim

nimboard :: [Int]
nimboard = [5,4,3,2,1]
showb :: [Int] -> IO ()
showb b = seqn [writeat (1,i) (show i ++ ":" ++ (concat (replicate x " *"))) | (i,x) <- (zip [1..5] b)]

nim :: [Int] -> IO ()
nim b = do
  cls
  if sum b == 0
    then do goto (1, 1)
            return ()
    else do showb b
            (row, num) <- niminput
            let v = current row
            if row `elem` nimboard && num <= v
              then nim (updatelist b (row - 1) (v - num))
              else nim b
            where
              current r = b !! (r - 1)

niminput :: IO (Int, Int)
niminput = do putStr "\nEnter row number: "
              r <- getChar
              putStr "\nEnter amount to take: "
              n <- getChar
              return (read [r], read [n])

updatelist :: [a] -> Int -> a -> [a]
updatelist xs i v = take i xs ++ [v] ++ drop (i + 1) xs

nimrun :: IO ()
nimrun = do cls
            showb nimboard
            nim nimboard

--------------------------------------------------------------------------------
-- 10. Declaring types and classes
--------------------------------------------------------------------------------
{-

-- type ----------------------------------------------------

type String = [Char]
type Board = [Pos]
type Pos = (Int, Int)

type Tree = (Int, [Tree])
-> Error. use 'data'

type Parser a = String -> [(a, String)]
type IO a     = World -> (a, World)
-}

type Assoc k v = [(k, v)]
find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k == k']

-- data ----------------------------------------------------

-- data Bool = False | True

data Move = West | East | North | South

move :: Move -> Pos -> Pos
move West  (x,y) = (x-1,y)
move East  (x,y) = (x+1,y)
move North (x,y) = (x,y-1)
move South (x,y) = (x,y+1)

moves :: [Move] -> Pos -> Pos
moves []     p = p
moves (m:ms) p = moves ms (move m p)

flip :: Move -> Move
flip West  = East
flip East  = West
flip North = South
flip South = North

data Shape = Circle Float | Rect Float Float
{-
> :t Circle
Circle :: Float -> Shape
> :t Rect
Rect :: Float -> Float -> Shape
-}

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just x

-- recursive types -----------------------------------------

data Nat = Zero | Succ Nat deriving (Show)
-- Zero
-- Succ Zero
-- Succ (Succ Zero)
-- Succ (Succ (Succ Zero))
-- ...

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

natadd :: Nat -> Nat -> Nat
natadd m n = int2nat (nat2int m + nat2int n)

natadd' Zero n     = n
natadd' (Succ m) n = Succ (natadd' m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree = Leaf Int | Node Tree Int Tree deriving (Show)
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
-- data Tree a = Node a [Tree a] -- in this case [] is Leaf

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r
-- > occurs 6 t
-- True

flatten :: Tree -> [Int]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n)     = m == n
occurs' m (Node l n r)
  | m == n      = True
  | m < n       = occurs m l
  | otherwise   = occurs m r

-- tautology test ------------------------------------------

data Prop = Const' Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval2 :: Subst -> Prop -> Bool
eval2 _ (Const' b)  = b
eval2 s (Var x)     = find' x s
eval2 s (Not p)     = not (eval2 s p)
eval2 s (And p q)   = eval2 s p && eval2 s q
eval2 s (Imply p q) = eval2 s p <= eval2 s q -- 'False < True' is True

vars :: Prop -> [Char]
vars (Const' _)  = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
-- > vars p4
-- "AABB"

bools :: Int -> [[Bool]]
bools n = map (map conv . make n . int2bin) [0..limit]
          where
            limit = (2^n) - 1
            make n bs = take n (bs ++ repeat 0)
            conv 0 = False
            conv 1 = True
-- > bools 2
-- [[False,False],[True,False],[False,True],[True,True]]

bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
           where bss = bools' (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools' (length vs))
           where vs = rmdups (vars p)
-- > substs p2
-- [[('A',False),('B',False)],[('A',True),('B',False)],[('A',False),('B',True)],[('A',True),('B',True)]]

isTaut :: Prop -> Bool
isTaut p = and [eval2 s p | s <- substs p]
-- > map isTaut [p1, p2, p3, p4]
-- [False,True,False,True]

-- virtual machine -----------------------------------------

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y
-- > value (Add (Add (Val 2) (Val 3)) (Val 4))
-- 9

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval3 :: Expr -> Cont -> Int
eval3 (Val n) c   = exec c n
eval3 (Add x y) c = eval3 x (EVAL y:c)

exec :: Cont -> Int -> Int
exec [] n         = n
exec (EVAL y:c) n = eval3 y (ADD n:c)
exec (ADD n:c) m  = exec c (n + m)

value' :: Expr -> Int
value' e = eval3 e []
-- > value' (Add (Add (Val 2) (Val 3)) (Val 4))
-- 9


-- class and instance
{-
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y) -- default definition of (/=). can be overwritten

instance Eq Bool where
  False  == False = True
  True   == True  = True
  _      == _     = False

class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max             :: a -> a ->
  min x y | x <= y    = x
          | otherwise = y
  max x y | x <= y    = y
          | otherwise = x

instance Ord Bool where
  False < True = True
  _ < _        = False
  b <= c       = (b < c) || (b == c)
  b > c        = c < b
  b >= c       = c <= b

data Bool = False | True
            deriving (Eq, Ord, Show, Read)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad Parser where
  ...
instance Monad IO where
 ...
-}

-- 10.8 (p.137) ----------------------------------------------------------------

-- 1 -------------------------------------------------------

natmult :: Nat -> Nat -> Nat
natmult Zero _     = Zero
natmult m (Succ n) = m `natadd'` (natmult m n)
-- > nat2int (natmult (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
-- 6

-- 2 -------------------------------------------------------

-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering
occurs1 :: Int -> Tree -> Bool
occurs1 m (Leaf n)     = m == n
occurs1 m (Node l n r) = case compare m n of
                           EQ -> True
                           LT -> occurs1 m l
                           GT -> occurs1 m r

-- 3 -------------------------------------------------------

data Tree' = Leaf' Int | Node' Tree' Tree' deriving Show
t1 :: Tree'
t1 = Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))
t1' = Node' (Node' (Leaf' 1) (Leaf' 2)) (Leaf' 3)
t1'' = Node' (Node' (Node' (Leaf' 0) (Leaf' 1)) (Leaf' 2)) (Leaf' 3)

balanced :: Tree' -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1
                       && balanced l
                       && balanced r

leaves :: Tree' -> Int
leaves (Leaf' _)   = 1
leaves (Node' l r) = leaves l + leaves r

-- 4 -------------------------------------------------------

balance :: [Int] -> Tree'
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r)
             where (l, r) = halve xs

p5 :: Prop
p5 = Or (Var 'A') (Var 'B')
p6 :: Prop
p6 = Equiv (Imply (Var 'A') (Var 'B')) (Or (Not (Var 'A')) (Var 'B'))

-- 5 -------------------------------------------------------

eval2' :: Subst -> Prop -> Bool
eval2' _ (Const' b)  = b
eval2' s (Var x)     = find' x s
eval2' s (Not p)     = not (eval2' s p)
eval2' s (And p q)   = eval2' s p && eval2' s q
eval2' s (Or p q)    = eval2' s p || eval2' s q
eval2' s (Imply p q) = eval2' s p <= eval2' s q
eval2' s (Equiv p q) = eval2' s p == eval2' s q

vars' :: Prop -> [Char]
vars' (Const' _)  = []
vars' (Var x)     = [x]
vars' (Not p)     = vars' p
vars' (And p q)   = vars' p ++ vars' q
vars' (Or p q)    = vars' p ++ vars' q
vars' (Imply p q) = vars' p ++ vars' q
vars' (Equiv p q) = vars' p ++ vars' q

substs' :: Prop -> [Subst]
substs' p = map (zip vs) (bools' (length vs))
           where vs = rmdups (vars' p)

isTaut' :: Prop -> Bool
isTaut' p = and [eval2' s p | s <- substs' p]

-- 6 -------------------------------------------------------

isTaut1 :: IO Bool
isTaut1 = do xs <- readLine
             case mparse logicExpr xs of
               [(p, [])] -> return (isTaut' p)
               [(p, xs)] -> do putStrLn ("Error parsing: " ++ xs)
                               return (isTaut' p)
               _  -> return False

-- logicExpr := logicTerm1 ('->' logicExpr | e)
-- logicTerm1 := logicTerm2 ('|' logicTerm1 | e)
-- logicTerm2 := logicFactor ('&' logicTerm2 | e)
-- logicFactor := '(' logicExpr ')' | '!' logicExpt | var
-- var := 'a' | 'b' | ...

logicExpr, logicFactor, logicTerm1, logicTerm2 :: MParser Prop
logicExpr = do lt1 <- logicTerm1
               do msymbol "->"
                  le <- logicExpr
                  return (Or (Not lt1) le)
                <|> return lt1

logicTerm1 = do lt2 <- logicTerm2
                do msymbol "|"
                   t1 <- logicTerm1
                   return (Or lt2 t1)
                 <|> return lt2

logicTerm2 = do lf <- logicFactor
                do msymbol "&"
                   lt2 <- logicTerm2
                   return (And lf lt2)
                 <|> return lf

logicFactor = do msymbol "("
                 le <- logicExpr
                 msymbol ")"
                 return le
              <|> do msymbol "!"
                     le <- logicExpr
                     return (Not le)
              <|> do c <- mtoken mletter
                     return (Var c)
-- > isTaut1
-- a&b->a
-- True
-- > isTaut1
-- a ->  a & b
-- False
-- > isTaut1
-- a & (a->b) -> b
-- True

newtype MParser a = P (String -> [(a,String)])

mparse :: MParser a -> String -> [(a,String)]
mparse (P p) inp = p inp

mitem :: MParser Char
mitem = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)])

-- Sequencing parsers
instance Functor MParser where
   fmap g p = P (\inp -> case mparse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative MParser where
   pure v = P (\inp -> [(v,inp)])
   pg <*> px = P (\inp -> case mparse pg inp of
                             []        -> []
                             [(g,out)] -> mparse (fmap g px) out)

instance Monad MParser where
   p >>= f = P (\inp -> case mparse p inp of
                           []        -> []
                           [(v,out)] -> mparse (f v) out)

-- Making choices
instance Alternative MParser where
   empty = P (const [])
   p <|> q = P (\inp -> case mparse p inp of
                           []        -> mparse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives
msat :: (Char -> Bool) -> MParser Char
msat p = do x <- mitem
            if p x then return x else empty

mdigit, mlower, mupper, mletter, malphanum :: MParser Char
mdigit    = msat isDigit
mlower    = msat isLower
mupper    = msat isUpper
mletter   = msat isAlpha
malphanum = msat isAlphaNum

mchar :: Char -> MParser Char
mchar x = msat (== x)

mstring :: String -> MParser String
mstring []     = return []
mstring (x:xs) = do mchar x
                    mstring xs
                    return (x:xs)

mident :: MParser String
mident = do x  <- mlower
            xs <- many malphanum
            return (x:xs)

mnat, mint :: MParser Int
mnat = do xs <- some mdigit
          return (read xs)
mint = do mchar '-'
          n <- mnat
          return (-n)
       <|> mnat

-- Handling spacing
mspace :: MParser ()
mspace = do many (msat isSpace)
            return ()

mtoken :: MParser a -> MParser a
mtoken p = do mspace
              v <- p
              mspace
              return v

midentifier :: MParser String
midentifier = mtoken mident

mnatural, minteger :: MParser Int
mnatural = mtoken mnat
minteger = mtoken mint

msymbol :: String -> MParser String
msymbol xs = mtoken (mstring xs)

-- 7 -------------------------------------------------------

data VMExpr = VMVal Int
            | VMAdd VMExpr VMExpr
            | VMMul VMExpr VMExpr

value1 :: VMExpr -> Int
value1 e = eval4 e []
-- > value1 (VMAdd (VMMul (VMVal 2) (VMVal 3)) (VMVal 4))
-- 10

type VMCont = [VMOp]
data VMOp = VM_EVALADD VMExpr | VM_ADD Int
          | VM_EVALMUL VMExpr | VM_MUL Int

eval4 :: VMExpr -> VMCont -> Int
eval4 (VMVal n)    c = exec' c n
eval4 (VMAdd x y)  c = eval4 x (VM_EVALMUL y:c)
eval4 (VMMul x y)  c = eval4 x (VM_EVALADD y:c)

exec' :: VMCont -> Int -> Int
exec' [] n               = n
exec' (VM_EVALADD y:c) n = eval4 y (VM_MUL n:c)
exec' (VM_EVALMUL y:c) n = eval4 y (VM_ADD n:c)
exec' (VM_ADD n:c)     m = exec' c (n + m)
exec' (VM_MUL n:c)     m = exec' c (n * m)

-- 8 -------------------------------------------------------
{-
-- * http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html#functors
-- * http://hackage.haskell.org/package/base-4.11.1.0/docs/src/GHC.Base.html

-- Maybe type
data Maybe' a = Nothing' | Just' a
instance Functor Maybe' where
  fmap f (Just' v) = Just' (f v)
  fmap f Nothing'  = Nothing'

instance Applicative Maybe' where
  pure = Just'
  Just' f  <*> m  = fmap f m
  Nothing' <*> _ = Nothing'

instance Monad Maybe' where
  return = Just'
  Nothing' >>= f = Nothing'
  Just' v >>= f  = f v

-- List type
instance Functor [] where
    fmap = map

instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Monad [] where
  return x = [x]
  xs >>= f = [y | x <- xs, y <- f x]
  m >>= f = concat . map f m
-}

--------------------------------------------------------------------------------
-- 11. The countdown problem
--------------------------------------------------------------------------------

data Op' = Add' | Sub' | Mul' | Div' deriving Show

valid :: Op' -> Int -> Int -> Bool
valid Add' _ _ = True
valid Sub' x y = x > y
valid Mul' _ _ = True
valid Div' x y = x `mod` y == 0

apply :: Op' -> Int -> Int -> Int
apply Add' x y = x + y
apply Sub' x y = x - y
apply Mul' x y = x * y
apply Div' x y = x `div` y

data Expr' = Val' Int | App Op' Expr' Expr' deriving Show

values :: Expr' -> [Int]
values (Val' n)    = [n]
values (App _ l r) = values l ++ values r
-- *Main> values (App Sub' (Val' 3) (Val' 2))
-- [3,2]

eval5 :: Expr' -> [Int]
eval5 (Val' n) = [n | n > 0]
eval5 (App o l r) = [apply o x y | x <- eval5 l,
                                   y <- eval5 r,
                                   valid o x y]
-- *Main> eval5 (App Sub' (Val' 3) (Val' 2))
-- [1]

-- http://d.hatena.ne.jp/kazu-yamamoto/comment/20081208/1228727709

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
-- *Main> subs [1,2]
-- [[],[2],[1],[1,2]]
-- *Main> subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-- *Main> interleave 1 [2,3,4]
-- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
-- *Main> perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))
-- *Main> choices [1,2]
-- [[],[2],[1],[1,2],[2,1]]
-- *Main> choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

solution :: Expr' -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval5 e == [n]
-- *Main> solution (App Mul' (App Add' (Val' 1) (Val' 2)) (Val' 3)) [1,2,3] 9
-- True

-- brute force

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]
-- *Main> split [1,2,3,4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

exprs :: [Int] -> [Expr']
exprs [] = []
exprs [n] = [Val' n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr' -> Expr' -> [Expr']
combine l r = [App o l r | o <- ops]

ops :: [Op']
ops = [Add', Sub', Mul', Div']

solutions :: [Int] -> Int -> [Expr']
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval5 e == [n]]
-- *Main> solutions [2,3] 6
-- [App Mul' (Val' 2) (Val' 3),App Mul' (Val' 3) (Val' 2)]

-- optimization 1

type Result = (Expr', Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val' n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]
solutions' ns n = [e | ns' <- choices ns,
                       (e, m) <- results ns',
                       m == n]
-- *Main> solutions [1,3,7,10,25,50] 765
-- ...
-- *Main> solutions [1,3,7,10,25,50] 831
-- []

-- optimization 2
-- x + y = y + x, x * y = y * x
-- x * 1 = x, x / 1 = x
valid' :: Op' -> Int -> Int -> Bool
valid' Add' x y = x <= y
valid' Sub' x y = x > y
valid' Mul' x y = x /= 1 && y /= 1 && x <= y
valid' Div' x y = y /= 1 && x `mod` y == 0

solutions'' ns n = [e | ns' <- choices ns,
                        (e, m) <- results' ns',
                        m == n]
results' [] = []
results' [n] = [(Val' n, n) | n > 0]
results' ns = [res | (ls, rs) <- split ns,
                     lx <- results' ls,
                     ry <- results' rs,
                     res <- combine'' lx ry]

combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

-- 11.7 (p.146) ----------------------------------------------------------------

-- 1 -------------------------------------------------------
-- choices xs = concat (map perms (subs xs))
choices' :: [a] -> [[a]]
choices' xs = [c | ss <- subs xs, c <- perms ss]

-- 2 -------------------------------------------------------
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice [_] []    = False
isChoice (x:xs) ys = elem x ys && isChoice xs (remove x ys)

remove :: Eq a => a -> [a] -> [a]
remove x [] = error "Target not found"
remove x (y:ys) | x == y    = ys
                | otherwise = y : remove x ys

-- 3 -------------------------------------------------------
-- infinite loop in expr.
split' []     = []
-- split' [_] = []
split' (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split' xs]
                                        -- expr' [1,2,3]
exprs' ns = [e | (ls, rs) <- split' ns, --      ([1,2,3], []) <- split' [1,2,3]
                 l <- exprs' ls,        -- expr' [1,2,3]
                 r <- exprs' rs,
                 e <- combine l r]

-- 4 -------------------------------------------------------
-- *Main> length [0 | cs <- choices [1,3,7,10,25,50], _ <- exprs cs]
-- 33665406
-- *Main> length [0 | cs <- choices [1,3,7,10,25,50], es <- exprs cs, _ <- eval5 es]
-- 4672540

-- 5 -------------------------------------------------------
valid1 Add' _ _ = True
valid1 Sub' x y = True
valid1 Mul' _ _ = True
valid1 Div' x y = y /= 0 && x `mod` y == 0

eval5' :: Expr' -> [Int]
eval5' (Val' n) = [n | n > 0]
eval5' (App o l r) = [apply o x y | x <- eval5' l,
                                    y <- eval5' r,
                                    valid1 o x y]
-- *Main> length [0 | cs <- choices [1,3,7,10,25,50], es <- exprs cs, _ <- eval5' es]
-- 10839369

-- 6 -------------------------------------------------------
data Op1 = Add1 | Sub1 | Mul1 | Div1 | Exp deriving Show
data Expr1 = Val1 Int | App' Op1 Expr1 Expr1 deriving Show
ops' = [Add1, Sub1, Mul1, Div1, Exp]
type Result' = (Expr1, Int)

valid2 :: Op1 -> Int -> Int -> Bool
valid2 Add1 x y = x <= y
valid2 Sub1 x y = True
valid2 Mul1 x y = x /= 1 && y /= 1 && x <= y
valid2 Div1 x y = y /= 1 && y /= 0 && x `mod` y == 0
valid2 Exp  x y = x /= 1 && y /= 1 && y >= 0

apply' :: Op1 -> Int -> Int -> Int
apply' Add1 x y = x + y
apply' Sub1 x y = x - y
apply' Mul1 x y = x * y
apply' Div1 x y = x `div` y
apply' Exp  x y = x ^ y

results1 :: [Int] -> [Result']
results1 [] = []
results1 [n] = [(Val1 n, n) | n > 0]
results1 ns = [res | (ls, rs) <- split ns,
                     lx <- results1 ls,
                     ry <- results1 rs,
                     res <- combine1 lx ry]
combine1 :: Result' -> Result' -> [Result']
combine1 (l,x) (r,y) = [(App' o l r, apply' o x y) | o <- ops', valid2 o x y]

solutions1 :: [Int] -> Int -> Either [Expr1] Int
solutions1 ns n = let rs = [r | ns' <- choices ns, r <- results1 ns']
                      match = [e | (e, m) <- rs, m == n]
                  in case match of
                       [] -> Right (findClosest n (map snd rs))
                       e  -> Left e

findClosest :: Int -> [Int] -> Int
findClosest n xs = let ds = [(abs (n - v), v) | v <- xs]
                       mindiff = minimum (map fst ds)
                   in head [v | (d, v) <- ds, mindiff == d]

expr2string :: Expr1 -> String
expr2string (Val1 n) = show n
expr2string (App' Add1 x y) = "(" ++ expr2string x ++ " + " ++ expr2string y ++ ")"
expr2string (App' Sub1 x y) = "(" ++ expr2string x ++ " - " ++ expr2string y ++ ")"
expr2string (App' Mul1 x y) = "(" ++ expr2string x ++ " * " ++ expr2string y ++ ")"
expr2string (App' Div1 x y) = "(" ++ expr2string x ++ " / " ++ expr2string y ++ ")"
expr2string (App' Exp  x y) = expr2string x ++ "^" ++ expr2string y
-- *Main> expr2string (App' Add1 (Val1 1) (App' Exp (Val1 3) (Val1 2)))
-- "(1 + 3^2)"

putExprs :: [Expr1] -> IO ()
putExprs [] = putStr ""
putExprs (e:es) = do putStrLn $ expr2string e
                     putExprs es

solve :: [Int] -> Int -> IO ()
solve ns n = case solutions1 ns n of
               Right i -> putStrLn $ show i
               Left es -> putExprs es

-- *main> solve [1,3,5,7] 100
-- (5 * ((3 * 7) - 1))
-- *Main> solve [1,3,7,10,25,50] 831
-- 832
-- *Main> solve [1,3,7,10,25,50] 765
-- (3 * ((7 * (50 - 10)) - 25))
-- ((25 - 10) * (1 + 50))
-- ((25 - (3 + 7)) * (1 + 50))
-- [...]

--------------------------------------------------------------------------------
-- 12. Lazy evaluation
--------------------------------------------------------------------------------

inf :: Int
inf = 1 + inf
-- infinite loop
-- (1 + (1 + (1 + (+ ... inf)))

-- *Main> fst (0, inf)
-- 0
-- *Main> repeat 1
-- [1,1,1,1,1,1,1,1,1,1,1,1,1, ...
-- *Main> head $ repeat 1
-- 1

-- replicate n x = take n (repeat x)

primes' :: [Int]
primes' = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- eager evaluation: $!

-- square $! (1+2)

sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x:xs) = sumwith (v + x) xs
-- sumwith 0 [1,2,3]
-- [...]
-- sumwith (((0 + 1) + 2) + 3) []
-- |
-- v

sumwith' v [] = v
sumwith' v (x:xs) = (sumwith $! (v + x)) xs
-- decreases the memory footprint

-- 12.9 (p.164) ----------------------------------------------------------------

-- 3

-- mult = \x -> (\y -> 3 * y)
--
-- mult 3 4
-- \x -> (\y -> x * y) 3 4
-- (\y -> 3 * y) 4
-- (3 * 4)
-- 12

-- 4

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- fibs = 0 : 1 : fibs' 0 1
-- fibs' p2 p1 = p : fibs' p1 p where p = p2 + p1

-- 5

fib :: Int -> Integer
fib n = fibs !! n

-- *Main> head $ dropWhile (<= 1000) fibs
-- 1597

-- 6

data Tree1 a = Leaf1 | Node1 (Tree1 a) a (Tree1 a) deriving Show

repeatT :: a -> Tree1 a
repeatT x = Node1 (repeatT x) x (repeatT x)

takeT :: Int -> Tree1 a -> Tree1 a
takeT 0 _ = Leaf1
takeT _ Leaf1 = Leaf1
takeT n (Node1 l x r) = Node1 (takeT (n - 1) l) x (takeT (n - 1) r)

replicateT :: Int -> a -> Tree1 a
replicateT n = takeT n . repeatT

--------------------------------------------------------------------------------
-- 13. Reasoning about programs
--------------------------------------------------------------------------------
{-

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
  |
  v
reverse' xs ys = reverse xs ++ ys {spec of reverse'}

base:
reverse' [] ys = reverse [] ++ ys {spec of reverse'}
               = [] ++ ys         {apply reverse}
               = ys               {apply ++}

recursive:
reverse' (x:xs) ys = reverse (x:xs) ++ ys      {spec of reverse'}
                   = (reverse xs ++ [x]) ++ y  {apply reverse}
                   = reverse xs ++ ([x] ++ ys) {associative law of ++}
                   = reverse xs ++ (x:ys)      {apply second ++}
                   = reverse' xs (x:ys)        {apply spec of reverse' inversely}
  |
  v
reverse' :: [a] -> [a] -> [a]
reverse' _ [] = []
reverse' (x:xs) ys = reverse' xs (x:ys)

-}

-- correctness of the compiler -----------------------------

data CExpr = CVal Int | CAdd CExpr CExpr deriving Show
ceval (CVal n) = n
ceval (CAdd x y) = ceval x + ceval y

type Stack = [Int]
type Code = [COp]
data COp = PUSH Int | CADD deriving Show

comp :: CExpr -> Code
comp (CVal n) = [PUSH n]
comp (CAdd x y) = comp x ++ comp y ++ [CADD]

cexec :: Code -> Stack -> Stack
cexec [] s = s
cexec (PUSH n:c) s = cexec c (n:s)
cexec (CADD:c) (m:n:s) = cexec c (n+m:s)

e :: CExpr
e = CAdd (CAdd (CVal 2) (CVal 3)) (CVal 4)
-- *Main> ceval e
-- 9
-- *Main> comp e
-- [PUSH 2,PUSH 3,CADD,PUSH 4,CADD]
-- *Main> cexec (comp e) []
-- [9]

{-

generalized correct behavior of the compiler:
  cexec (comp e) [] = [ceval e]
make it handle arbitrary stack →
  cexec (comp e) s  =  ceval e : s

when e = CVal n:

  {LHS} = cexec (comp (CVal n)) s {apply comp}
        = cexec [PUSH n] s        {apply cexec}
        = cexec [] (n:s)
        = n:s

  {RHS} = ceval (CVal n) : s {apply ceval}
        = n:s

when e = x and e = y, assume the followings hold:

  cexec (comp x) s = ceval x:s
  cexec (comp y) s = ceval y:s

when e = CAdd x y:

  {LHS} = cexec (comp (CAdd x y)) s                  {apply ccomp}
        = cexec (comp x ++ comp y ++ [ADD]) s        { associative law of ++}
        = cexec (comp x ++ (comp y ++ [ADD])) s      { distributive property}
        = cexec (comp y ++ [ADD]) (cexec (comp x) s) { from the assumption on x}
        = cexec (comp y ++ [ADD]) (ceval x:s)        { distributive property}
        = cexec [ADD] (cexec (comp y) (ceval x:s))   { from the assumption on y}
        = cexec [ADD] (ceval y : ceval x:s)          { apply cexec}
        = (ceval x + ceval y) : s

  {RHS} = ceval (CAdd x y) : s    {apply ceval}
        = (ceval x + ceval y) : s
□

----

-- comp' e c = comp e ++ c
comp' :: CExpr -> Code -> Code
comp' (CVal n) c = PUSH n:c
comp' (CAdd x y) c = comp' x (comp' y (ADD:c))

→ comp e = comp' e []

----

prove: cexec (comp' e c) s = cexec c (ceval e:s)

when e = CVal n:
  {LHS} = cexec (comp' (CVal n) c) s {apply comp'}
        = cexec (PUSH n:c) s         {apply cexec}
        = cexec c (n:s)

  {RHS} = cexec c (ceval (CVal n):s) {apply ceval}
        = cexec c (n:s)

when e = x and e = y, assume the followings hold:
  cexec (comp' x c) s = cexec c (ceval x:s)
  cexec (comp' y c) s = cexec c (ceval y:s)

when e = CAdd x y:
  {LHS} = cexec (comp' (CAdd x y) c) s         {apply comp'}
        = cexec (comp' x (comp' y (CADD:c))) s {from the assumption on x}
        = cexec (comp' y (CADD:c)) (ceval x:s) {from the assumption on y}
        = cexec (CADD:c) (ceval y : ceval x:s) {apply cexec}
        = cexec c ((ceval x + ceval y) : s)

  {RHS} = cexec c (ceval (CAdd x y) : s)    {apply ceval}
        = cexec c ((ceval x + ceval y) : s)
□

-}

-- 13.9 (p.184) ----------------------------------------------------------------
{-

-- 1 -------------------------------------------------------

-- 0 and (n+1) pettern:
(^) :: (Num a, Integral b) => a -> b -> a
_ (^) 0     = 1
x (^) (n+1) = x * (x^n)
 
(!!) :: [a] -> Int -> a
(x:_) !! 0      = x
(x:xs) !! (n+1) = xs !! n

take, drop

-- [x] and (x:xs) pattern:
last, init, foldr1

-- otherwise in gurad
toLower, toUpper,
takeWhile, dropWhile

-- 2 -------------------------------------------------------

-- add :: Nat -> Nat -> Nat
-- add Zero m = m
-- add (Succ n) m = Succ (add n m)

prove: add n (Succ m) = Succ (add n m)

when n = Zero:
  {LHS} = add Zero (Succ m)
        = Succ m
  {RHS} = Succ (add Zero m)
        = Succ m = {LHS}

when n = x, assume the following holds:
  add x (Succ m) = Succ (add x m)

when n = Succ x:
  {LHS} = add (Succ x) (Succ m)
        = Succ (add x (Succ m)) {from the assumption}
        = Succ (Succ (add x m))
  {RHS} = Succ (add (Succ x) m)
        = Succ (Succ (add x m)) = {LHS}
□

-- 3 -------------------------------------------------------

prove: add n m = add m n

when n = Zero:
  {LHS} = add Zero m
        = m
  {RHS} = add m Zero
        = m = {LHS}

when n = x, assume the following holds:
  add x m = add m x

when n = Succ x:
  {LHS} = add (Succ x) m
        = Succ (add x m) {from assumption}
        = Succ (add m x)
  {RHS} = add m (Succ x) {from 2}
        = Succ (add m x) = {LHS}
□

-- 4 -------------------------------------------------------

-- replicate 0 _     = []
-- replicate (n+1) c = c : replicate n c

-- all p []     = True
-- all p (x:xs) = p x && all p xs

prove: all (== x) (replicate n x) = True

when n = 0:
  {LHS} = all (== x) (replicate 0 x)
        = all (== x) []
        = True = {RHS}

when n = y, assume the following holds:
  all (== x) (replicate y x) = True

when n = y + 1
  {LHS} = all (== x) (replicate (y+1) x)
        = all (== x) (x : replicate y x)
        = True && all (== x) (replicate y x) {from the assumption}
        = True && True
        = True = {RHS}
□

-- 5 -------------------------------------------------------

-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

prove: xs ++ [] = xs

when xs = []:
  {LHS} = [] ++ []
        = [] = {RHS}

when xs = ns assume the following holds:
  ns ++ [] = ns

when xs = (n:ns):
  {LHS} = (n:ns) ++ []
        = n : (ns ++ []) {from the assumption}
        = n:ns = {RHS}
□

prove: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

when xs = []:
  {LHS} = [] ++ (ys ++ zs)
        = ys ++ zs
  {RHS} = ([] ++ ys) ++ zs
        = ys ++ zs = {LHS}

when xs = ns assume the following holds:
  ns ++ (ys ++ zs) = (ns ++ ys) ++ zs

when xs = (n:ns):
  {LHS} = (n:ns) ++ (ys ++ zs) 
        = n : (ns ++ (ys ++ zs)) {from the assumption}
        = n : (ns ++ ys) ++ zs
  {RHS} = ((n:ns) ++ ys) ++ zs
        = n : (ns ++ ys) ++ zs
□

-- 6 -------------------------------------------------------

the distributive property used in 13.5's proof:

  reverse (xs ++ ys) = reverse ys ++ reverse xs

is more general and basic property (i.e. one of commutative, associative,
distributive property) than this lemma:

  reverse (xs ++ [x]) = x : reverse (xs)

therefore better (?)

-- 7 -------------------------------------------------------

-- map f [] = []
-- map f (x:xs) = f x : map f x
-- (f.g) x = f (g x)

prove: map f (map g xs) = map (f.g) xs

when xs = []:
  {LHS} = map f (map g [])
        = map f []
        = []
  {RHS} = map (f.g) []
        = [] = {LHS}

when xs = ns assume the following holds:
  map f (map g ns) = map (f.g) ns

when xs = (n:ns):
  {LHS} = map f (map g (n:ns))
        = map f (g n : map g ns)
        = f (g n) : map f (map g ns) {from the assumption}
        = f (g n) : map (f.g) ns
  {RHS} = map (f.g) (n:ns)
        = (f.g) n : map (f.g) ns
        = f (g n) : map (f.g) ns = {LHS}
□

-- 8 -------------------------------------------------------

-- take 0 _ = []
-- take (n+1) [] = []
-- take (n+1) (x:xs) = x : take n xs

-- drop 0 xs = xs
-- drop (n+1) [] = []
-- drop (n+1) (_:xs) = drop n ns

prove: take n xs ++ drop n xs = xs

when n = 0:
  {LHS} = take 0 xs ++ drop 0 xs
        = [] ++ xs
        = xs = {RHS}
when xs = []:
  {LHS} = take n [] ++ drop n []
        = [] ++ []
        = [] = {RHS}

when n = m and xs = ns, assume the following holds:
  take m ns ++ drop m ns = ns

when n = m+1 and xs = (n:ns):
  {LHS} = take (m+1) (n:ns) ++ drop (m+1) (n:ns)
        = n : (take m ns) ++ drop (m+1) (n:ns)
        = n : (take m ns) ++ drop m ns
        = n : (take m ns ++ drop m ns)           {from the assumption}
        = n:ns = {RHS}
□

-- 9 -------------------------------------------------------

data Tree = Leaf Int | Node Tree Tree

prove: (number of leavs) = (number of nodes + 1)

(number of leaves):
  leaves :: Tree -> Int
  leaves (Leaf _)   = 1
  leaves (Node l r) = leaves l + leaves r

(number of nodes):
  nodes :: Tree -> Int
  nodes (Leaf _)   = 0
  nodes (Node l r) = 1 + nodes l + nodes r

prove': leaves t = (nodes t) + 1

when t = Leaf n:
  {LHS} = leaves (Leaf n)
        = 1
  {RHS} = (nodes (Leaf n)) + 1
        = 0 + 1
        = 1 = {LHS}

when t = Node l r, assume the following holds:
  leaves (Node l r) = nodes (Node l r) + 1

when t = Node (Node l r) (Node l r):
  {LHS} = leaves Node (Node l r) (Node l r)
        = leaves (Node l r) + leaves (Node l r) {from the assumption}
        = nodes (Node l r) + 1 + nodes (Node l r) + 1
  {RHS} = nodes (Node (Node l r) (Node l r)) + 1
        = 1 + nodes (Node l r) + nodes (Node l r) + 1
        = {LHS}
□

-- 10 -------------------------------------------------------

-- derive recursive definition of comp'

-- comp' e c = comp e ++ c

base part:
    comp' (Val n) c
  = comp (Val n) ++ c
  = [PUSH n] ++ c
  = PUSH n : c

recursive part:
    comp' (ADD x y) c
  = comp (ADD x y) ++ c
  = comp x ++ comp y ++ [ADD] ++ c
  = comp x ++ (comp y ++ (ADD:c))
  = comp x ++ (comp' y (ADD:c))
  = comp' x (comp' y (ADD:c))

  |
  v

comp' :: Expr -> Code -> Code
comp' (Val n) c   = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD:c))

-}
