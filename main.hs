module Main where

import System.IO
import Set_1 (fact, power2, nRoots)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- Importante para que Docker envíe el texto al instante
    putStrLn "\n======================================="
    putStrLn "  Haskell Portfolio λ - Javier G. Santana"
    putStrLn "  Checkout my repo @ https://github.com/javiergarciasantana/haskell_functions"
    putStrLn "======================================="
    putStrLn "1. Compute the Factorial of a number"
    putStrLn "2. Compute the power of 2 given the exponent"
    putStrLn "3. Exit"
    putStr "Chose an option: "
    
    option <- getLine
    case option of
        "1" -> do
            putStr "Numerical input: "
            nStr <- getLine
            let n = read nStr :: Integer
            putStrLn $ "-> The Factorial of " ++ show n ++ " is " ++ show (fact n)
            main -- Vuelve al menú
        "2" -> do
            putStr "Numerical input: "
            nStr <- getLine
            let n = read nStr :: Integer
            putStrLn $ "-> 2 to the power of " ++ show n ++ " is " ++ show (power2 n)
            main -- Vuelve al menú
        "3" -> putStrLn "Exiting the program..."
        _   -> putStrLn "Non valid option." >> main