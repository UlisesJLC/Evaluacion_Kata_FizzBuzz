import Data.List (intercalate)
import Data.Char (digitToInt)

-- Función para convertir un número a su representación en letras
numeroEnLetras :: Int -> String
numeroEnLetras n
    | n <= 0 = "cero"
    | otherwise = traducirNumero n

-- Función para traducir un número a palabras
traducirNumero :: Int -> String
traducirNumero n = traducirNumeroAux n 0

-- Función auxiliar que traduce un número tomando en cuenta la posición
traducirNumeroAux :: Int -> Int -> String
traducirNumeroAux 0 _ = ""
traducirNumeroAux n pos
    | n < 1000 = traducirNumeroAux (div n 1000) (pos + 1) ++ " " ++ traducirGrupo (mod n 1000) pos
    | otherwise = error "Número fuera de rango"

-- Función que traduce un grupo de tres dígitos a palabras
traducirGrupo :: Int -> Int -> String
traducirGrupo 0 _ = ""
traducirGrupo n pos
    | n < 100 = traducirDecenas (div n 10) ++ " " ++ traducirUnidades (mod n 10) pos
    | n < 1000 = traducirCentenas (div n 100) ++ " " ++ traducirDecenas (mod (div n 10) 10) ++ " " ++ traducirUnidades (mod n 10) pos
    | otherwise = error "Número fuera de rango"

-- Función que traduce las centenas a palabras
traducirCentenas :: Int -> String
traducirCentenas 0 = ""
traducirCentenas c = unidades !! (c - 1) ++ " cientos"

-- Función que traduce las decenas a palabras
traducirDecenas :: Int -> String
traducirDecenas 0 = ""
traducirDecenas 1 = especiales !! 0
traducirDecenas d = decenas !! (d - 1)

-- Función que traduce las unidades a palabras, considerando la posición
traducirUnidades :: Int -> Int -> String
traducirUnidades 0 _ = ""
traducirUnidades u pos
    | pos == 0 = especiales !! (u - 1)
    | otherwise = unidades !! (u - 1)

-- Arreglos de palabras
unidades = ["uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve"]
especiales = ["once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]
decenas = ["diez", "veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

-- Función para verificar si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    | n <= 1 = False
    | otherwise = null [x | x <- [2..floor(sqrt(fromIntegral n))], n `mod` x == 0]

main :: IO ()
main = do
    putStrLn "Ingrese un número:"
    input <- getLine
    let numero = read input :: Int
    if numero >= 1 && numero <= 1000
        then if esPrimo numero
            then putStrLn "FizzBuzz"
            else putStrLn $ numeroEnLetras numero
        else putStrLn "Número fuera de rango"
