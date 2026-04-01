--Author: Javier Garcia Santana
--Title: Excersise Set 4 Functional Programming

module Set_4 where

import Data.Char

--Exercise 1
data GTree a = Leaf a | Gnode [GTree a] deriving (Show)

-- Return the depth of a GTree
depthGTree :: GTree a -> Int
depthGTree (Leaf _) = 1
depthGTree (Gnode subtrees) = 1 + maximum (map depthGTree subtrees)

-- Check if an element occurs in a GTree
elemInGTree :: Eq a => a -> GTree a -> Bool
elemInGTree x (Leaf value) = x == value
elemInGTree x (Gnode subtrees) = any (elemInGTree x) subtrees

-- Map a given function over the elements at the leaves of a GTree
mapLeaves :: (a -> b) -> GTree a -> GTree b
mapLeaves f (Leaf value) = Leaf (f value)
mapLeaves f (Gnode subtrees) = Gnode (map (mapLeaves f) subtrees)


--Exercise 2

-- Define the Expr data structure
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char

-- Define the Valuation type
type Valuation a = [(Var, a)]

-- Write the eval function
eval :: Valuation a -> Expr a -> a
eval _ (Lit x) = x
eval vars (EVar var) = case lookup var vars of
  Just val -> val
  Nothing  -> error ("Variable " ++ [var] ++ " not found in valuation.")
eval vars (Op op exprs) = op (map (eval vars) exprs)


--Exercise 3

splits :: String -> [(String, String)]
splits str = zip (inits str) (tails str)
  where
    inits [] = []
    inits (x:xs) = let prefix = x : head (inits xs) in prefix : inits xs

    tails [] = []
    tails (x:xs) = (x:xs) : tails xs

type RegExp = String -> Bool

epsilon ::  RegExp
epsilon = (=="")

char ::  Char -> RegExp
char ch = (==[ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x

(<*>) :: RegExp -> RegExp -> RegExp 
e1 <*> e2 = \x -> 
    or [e1 y && e2 z | (y,z) <- splits x]
      
star ::  RegExp -> RegExp
star p = epsilon ||| (p Set_4.<*> star p)

-- Function to match zero or one occurrences of a pattern
option :: RegExp -> RegExp
option p = epsilon ||| p

-- Function to match one or more occurrences of a pattern
plus :: RegExp -> RegExp
plus p = p Set_4.<*> star p

--Exercise 4?

data NumList a = Nlist [a] deriving (Show)

instance (Num a, Eq a, Fractional a) => Eq (NumList a) where
  (Nlist xs) == (Nlist ys) = abs (average xs - average ys) < epsilon
    where epsilon = 1e-10

instance (Num a, Ord a, Fractional a) => Ord (NumList a) where
  compare (Nlist xs) (Nlist ys) = compare (average xs) (average ys)

-- Helper function to calculate the average of a list
average :: (Num a, Fractional a) => [a] -> a
average [] = 0
average xs = sum xs / fromIntegral (length xs)


--Exercise 5
data Result a = OK a | Error String deriving (Show)

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f g x =
  case f x of
    OK y    -> g y
    Error e -> Error e

-- Example functions that return Result
divideBy :: Int -> Int -> Result Double
divideBy _ 0 = Error "Division by zero"
divideBy x y = OK (fromIntegral x / fromIntegral y)

sqrtResult :: Double -> Result Double
sqrtResult x
  | x < 0     = Error "Square root of a negative number"
  | otherwise = OK (sqrt x)

-- Compose the functions using composeResult
composedFunction :: Int -> Result Double
composedFunction = composeResult (divideBy 10) sqrtResult


--Exercise 6

-- Helper function to generate an infinite list of prime numbers
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

goldbach :: Integer -> Bool
goldbach n = all isEvenSum [4,6..n]
  where
    isEvenSum x = any isPrimePair [(a, x - a) | a <- [2..x `div` 2]]
    isPrimePair (a, b) = isPrime a && isPrime b
    isPrime num = all (\d -> num `mod` d /= 0) [2..floor (sqrt (fromIntegral num))]

-- Example using an imaginary `primes` function
-- primes :: [Integer]
-- primes = ... -- implementation not shown


--Exercise 7

data Stream a = Cons a (Stream a)

streamtoList :: Stream a -> [a]
streamtoList (Cons x xs) = x : streamtoList xs

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)
