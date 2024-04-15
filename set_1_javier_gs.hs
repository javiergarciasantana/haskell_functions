module Set_1 where

import  Test.QuickCheck


nAnd :: Bool -> Bool -> Bool
nAnd x y = not(x && y)

nAnd2 :: Bool -> Bool -> Bool
nAnd2 _ False = True
nAnd2 x True = not(x)

nAnd3 :: Bool -> Bool -> Bool
nAnd3 True True = False
nAnd3 False False = True
nAnd3 False True = True
nAnd3 True False = True

prop_nAnd x y = nAnd x y == nAnd2 x y && nAnd x y == nAnd3 x y

prop_nAnd_extra x y = nAnd x True == not(x)

nDigits :: Integer -> Int
nDigits n 
  | n >= 0 = length(show n)
  | n < 0 = length(show (-n))
 
nRoots :: Float -> Float -> Float -> Int
nRoots a b c 
  | a==0 = error "the first argument should be non-zero!"
  | b^2 > 4.0 * a * c = 2
  | b^2 == 4.0 * a * c = 1
  | b^2 < 4.0 * a * c = 0

root_add :: Float -> Float -> Float -> Float
root_add a b c = ((-b) + sqrt((b^2) + (-4) * a * c)) / (2 * a)

root_subs :: Float -> Float -> Float -> Float
root_subs a b c = ((-b) - sqrt((b^2) + (-4) * a * c)) / (2 * a)

smallerRoot :: Float -> Float -> Float -> Float 
smallerRoot a b c  
  | (nRoots a b c == 0) = error "no roots for this equation"
  | (nRoots a b c == 2) && root_add a b c < root_subs a b c = root_add a b c
  | (nRoots a b c == 2) && root_add a b c > root_subs a b c = root_subs a b c
  | otherwise = ((-b)) / (2 * a)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c  
  | (nRoots a b c == 0) = error "no roots for this equation"
  | (nRoots a b c == 2) && root_add a b c > root_subs a b c = root_add a b c
  | (nRoots a b c == 2) && root_add a b c < root_subs a b c = root_subs a b c
  | otherwise = ((-b)) / (2 * a)

power2 :: Integer -> Integer
power2 x
  | x > 0 = 2 * power2(x-1)        
  | x == 0 = 1 
  | x < 0 = 0
  | otherwise = error "Not a natural number"

mult :: Integer -> Integer -> Integer --fixed!!
mult m n
  | n > 0 = m + (mult (m) (n-1)) 
  | (n == 0) || (m == 0) = 0
  | n < 0 = (-m) + (mult (m) (n+1)) 

prod :: Integer -> Integer -> Integer 
prod m n 
  | m < n = n * (prod(m) (n-1))
  | n - m == 0 = m
  | m > n = error "Invalid range"

fact :: Integer -> Integer 
fact n
  | n == 0 = 1
  | n > 0 = prod 1 n
  | n < 0 = error "Invalid value"