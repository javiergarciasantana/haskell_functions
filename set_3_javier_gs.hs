--Author: Javier Garcia Santana
--Title: Excersise Set 3 Functional Programming

module Set_3 where

import Data.Char

--Excersise 1
data Shape = Circle Float (Float, Float) | Rectangle Float Float (Float, Float)

area ::  Shape -> Float
area (Circle r _) = pi*r*r
area (Rectangle h w _) = h*w

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

overlap :: Shape -> Shape -> Bool
overlap (Circle r1 point1) (Circle r2 point2) = distance point1 point2 <= r1 + r2
overlap (Rectangle w1 h1 (x1, y1)) (Rectangle w2 h2 (x2, y2)) = not (x1 + w1 < x2 || x1 > x2 + w2 || y1 + h1 < y2 || y1 > y2 + h2)
overlap (Circle r1 point1) (Rectangle w1 h1 (x1, y1)) = error "not able to compare this two shapes yet"
overlap (Rectangle w1 h1 (x1, y1)) (Circle r1 point1) = error "not able to compare this two shapes yet"

--Excersise 2
myFilterAny :: (a->Bool) -> [a] -> Bool
myFilterAny cond arr = not (null (filter cond arr))

myFilterAll :: (a->Bool) -> [a] -> Bool
myFilterAll cond arr = (null (filter (not . cond) arr))--comparing two lists?

myMapAny :: (a->Bool) -> [a] -> Bool
myMapAny cond arr = not (null (filter (==True) (map cond arr)))

myMapAll :: (a->Bool) -> [a] -> Bool
myMapAll cond arr = (null (filter (==False) (map cond arr)))

myFoldrAny :: (a->Bool) -> [a] -> Bool
myFoldrAny cond arr = (foldr (||) (False) (map cond arr))

myFoldrAll :: (a->Bool) -> [a] -> Bool
myFoldrAll cond arr = (foldr (&&) (True) (map cond arr))

--Excersise 3
myUnzip :: [(a,b)] -> ([a],[b])
myUnzip = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])

--Excersise 4
myLength :: [a] -> Integer
myLength arr = sum (map (\x->1) arr)

myLength2 :: [a] -> Integer
myLength2 arr = foldr (+) 0 (map (\x->1) arr)

--Excersise 5
ff :: Integer -> [Integer] -> Integer--can be improved?
ff maxNum arr
  | sum (map ((10*) . abs) arr) <= maxNum = sum (map ((10*) . abs) arr)
  | Prelude.otherwise = error "Out of range"

--Excersise 6 
total :: (Integer -> Integer) -> Integer -> Integer
total cond n 
  | n > 0 = foldr (+) 0 (map cond [0..n])
  | Prelude.otherwise = error "Size equal or less than zero"

--Excersise 7
square :: Integer -> Integer
square x = x * x

iter :: Integer -> (a -> a) -> (a -> a)
iter n f 
  | n <= 0 = id
  | Prelude.otherwise = \x -> f (iter (n - 1) f x)

iter2 :: Integer -> (a -> a) -> (a -> a)
iter2 n f 
  | n <= 0 = id
  | otherwise = foldr (.) id (replicate (fromInteger n) f)

--Excersise 8
splits :: [a] -> [([a],[a])]
splits str = map (\x -> (take x str, drop x str)) [0 .. length str]

