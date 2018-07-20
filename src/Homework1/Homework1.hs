---------------------------------------
-- dcalo
-- 07/02/2018
-- CIS194 
---------------------------------------
module Homework1.Homework1 where -- forma correcta para declarar un modulo dentro de un folder

-- VALIDATE CREDIT CARD
--Metodo de prueba general, recordar que haskell se basa en LAZY no se llama a todas las funciones solo las que necesita cuando lo necesita 

-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validateCreditCard :: Integer -> Bool
validateCreditCard = validate . sumDigits . doubleEveryOther . toDigits

-- Helper Functions
-- 
--Exercise 1: 
toDigits :: Integer -> [Integer] --definicion de una funsion en haskell
-- toDigits 0 = []
toDigits n
    | n < 1          = []
    -- | n < 10          = [n] -- no es necesario / redundante  -- a `div` b se usa en enteros a / b se usa en doubles
    | otherwise      = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev:: Integer -> [Integer]
toDigitsRev n 
    | n < 1          = []
    -- | n < 10          = [n] -- no es necesario / redundante -- a `div` b se usa en enteros a / b se usa en doubles  
    | otherwise      = (n `mod` 10) : toDigitsRev (n `div` 10)

--Exercise 2:
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []                          -- conjunto vacio
doubleEveryOther [x]        = [x]                         -- asignar a si mismo
doubleEveryOther (x:y:z) = (2*x): y : doubleEveryOther z  -- NO EXISTEN objetos en haskell, existen TIPOS ( el tipo x y z), como en conjuntos, similar a una variable

--Exercise 3:
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:y:z) 
    | x > 9             = (sumDigits . toDigits) x + y + sumDigits z --Pensar mas como HASKELL. Agrupar funciones y permitir que reciban el parametro (sumDigits .toDigits es el metodo agrupado, x+y+sumDigits z es el nuevo parametro)
    | otherwise         = x + y + sumDigits z -- z es un paso de parametro directo

--Exercise 4:
validate :: Integer -> Bool
validate n = n `mod` 10 == 0

--THE TOWERS OF HANOI
--Excersie 5
type Peg = String  --defincion de tipos TYPE
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move] -- la funsion puede tener muchos argumantos, el ultimo tipo declarado es la salida de la funsion (para el caso la salida es [Move])
hanoi 1 from to aux = [(from,to)]
hanoi n from to aux = 
    hanoi (n-1) from aux to ++ [(from,to)] ++ hanoi (n-1) aux to from