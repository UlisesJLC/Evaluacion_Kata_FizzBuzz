module FizzBuzz (
    fizzBuzz,
    esPrimo,
    numeroEnPalabras
) where

import Data.List (intercalate)

fizzBuzz :: Int -> String
fizzBuzz n
    | esPrimo n = "FizzBuzz"
    | otherwise = numeroEnPalabras n

esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..sqrtInt n]
    where sqrtInt = ceiling . sqrt . fromIntegral

numeroEnPalabras :: Int -> String
numeroEnPalabras n
    | n < 20              = numerosHasta20 !! n
    | n == 20             = "veinte"
    | n < 30              = "veinti" ++ numerosEnPalabras (n - 20)
    | n < 100 && n `mod` 10 == 0 = decenas !! (n `div` 10 - 2)
    | n < 100             = decenas !! (n `div` 10 - 2) ++ " y " ++ numerosEnPalabras (n `mod` 10)
    | n == 100            = "cien"
    | n < 200             = "ciento " ++ casoDecenas (n - 100)
    | n < 1000 && n `mod` 100 == 0 = cientos !! (n `div` 100 - 1)
    | n < 1000            = casoCentenas n
    | n == 1000           = "mil"
    | n == 1001           = "mil uno" 
    | n < 2000            = "mil " ++ numeroEnPalabras (n `mod` 1000)
    | n < 1000000         = casoMiles (n `div` 1000) ++ " " ++ casoCentenas (n `mod` 1000)
    | n < 1000000000      = casoMillones (n `div` 1000000) ++ " " ++ casoMiles ((n `mod` 1000000) `div` 1000) ++ " " ++ casoCentenas (n `mod` 1000)
    | otherwise           = "Numero no soportado"
    where
        numerosHasta20 = ["cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce", "trece", "catorce", "quince", "dieciseis", "diecisiete", "dieciocho", "diecinueve"]
        decenas = ["veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]
        cientos = ["ciento", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]
        numerosEnPalabras m = numerosHasta20 !! m
        casoDecenas m
            | m == 0         = ""
            | m < 100        = numeroEnPalabras m
            | m < 200        = "ciento " ++ casoDecenas (m `mod` 100)
            | otherwise      = cientos !! ((m `div` 100) - 1) ++ " " ++ casoDecenas (m `mod` 100)
        casoCentenas m
            | m == 100          = "cien"
            | m < 200           = "ciento " ++ casoDecenas (m `mod` 100)
            | m `mod` 100 == 0  = cientos !! ((m `div` 100) - 1)
            | otherwise         = cientos !! ((m `div` 100) - 1) ++ " " ++ casoDecenas (m `mod` 100)
        casoMiles m
            | m == 0    = ""
            | m == 1    = "mil"
            | otherwise = numeroEnPalabras m ++ " mil"
        casoMillones m
            | m == 1    = "un millon"
            | otherwise = numeroEnPalabras m ++ " millones"
