--Author: Javier Garcia Santana
--Title: Excersise Set 2 Functional Programming

module Set_2 where

import  Test.QuickCheck
import Data.Char

--excersise 1
average :: [Float] -> Float
average [] = error "Empty list!"
average xs = sum xs / fromIntegral (length xs)

--excersise 2
divides :: Integer -> [Integer]
divides n 
  | n == 0 = [0]
  | n >= 1 = divide n n
  | otherwise = divide (abs n) (abs n)
  where
   divide _ 1 = [1]
   divide x y
    | x `mod` y == 0 = y : divide x (y - 1)
    | otherwise = divide x (y - 1)

divides2 :: Integer -> [Integer]
divides2 n 
  | n == 0 = [0]
  | n >= 1 = [x | x <- [1..n], n `mod` x == 0]
  | otherwise = divides2 (abs n)

isprime :: Integer -> Bool
isprime n 
 | length (divides n) == 2 && n > 1 = True
 | otherwise = False

--excersise 3
prefix :: String -> String -> Bool
prefix _ [] = False
prefix [] _ = True
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substring :: String -> String -> Bool
substring _ [] = False
substring [] _ = True
substring (x:xs) (y:ys)  
  | x == y = prefix (x:xs) ys || substring xs (y:ys)
  | otherwise = substring (x:xs) ys

--excersise 4
ins ::  Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
  | x <= y = x:(y:ys)
  | otherwise = y:(ins x ys)

iSort ::  [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

permut :: [Integer] -> [Integer] -> Bool
permut xs ys = (iSort xs == iSort ys)

--excersise 5
capitalise :: String -> String
capitalise n = [toUpper ch | ch <- n, (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')]

--excersise 6
-- itemTotal [("Banana", 9.0), ("Apple", 3.0), ("MApple", 4.0), ("Banana", 2.0)]
itemTotal :: [(String,Float)] -> [(String,Float)]
itemTotal [] = []
itemTotal ((item, price):rest) = mergeItem item price rest
  where 
    mergeItem _ _ [] = []
    mergeItem item price ((item', price'):rest')
      | item == item' = mergeItem item' (price + price') rest'
      | otherwise = (item, price) : mergeItem item' price' rest'


itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)]
itemDiscount _ _ [] = []
itemDiscount item_name discount ((item, price):rest)
  | discount >= 0 && discount <= 100 = applyDiscount item_name discount item price rest
  | otherwise = error "Discount value out of range"
  where
    applyDiscount _ _ _ _ [] = []
    applyDiscount item_name discount item price ((item', price'):rest')
     | item_name == item = (item, (price - ((price * fromIntegral (discount)) / 100))) : applyDiscount item_name discount item' price' rest'
     | otherwise = (item, price) : applyDiscount item_name discount item' price' rest'
