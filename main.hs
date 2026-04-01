module Main where

import System.IO
import Set_1 (fact, power2, nRoots)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- Importante para que Docker envíe el texto al instante
    putStrLn "\n======================================="
    putStrLn "  Haskell Portfolio - Javier G. Santana"
    putStrLn "======================================="
    putStrLn "1. Calcular Factorial (Set 1)"
    putStrLn "2. Calcular Potencia de 2 (Set 1)"
    putStrLn "3. Salir"
    putStr "Elige una opción: "
    
    option <- getLine
    case option of
        "1" -> do
            putStr "Introduce un número: "
            nStr <- getLine
            let n = read nStr :: Integer
            putStrLn $ "-> El factorial de " ++ show n ++ " es " ++ show (fact n)
            main -- Vuelve al menú
        "2" -> do
            putStr "Introduce un número: "
            nStr <- getLine
            let n = read nStr :: Integer
            putStrLn $ "-> 2 elevado a " ++ show n ++ " es " ++ show (power2 n)
            main -- Vuelve al menú
        "3" -> putStrLn "Saliendo del simulador..."
        _   -> putStrLn "Opción no válida." >> main