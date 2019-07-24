-- :l /home/diego/Documents/github/EstructurasDiscretasPracticas/practica3/Binario.hs

module Binario where

--Ejercicio 1.1
data Binario = BaseUno | Cero Binario | Uno Binario deriving(Read, Eq)

instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

--Ejercicio 1.2
natToBin :: Int -> Binario
natToBin 1 = BaseUno
natToBin n = if((mod n 2) == 0) then Cero (natToBin (div n 2))
             else Uno (natToBin (div n 2))

--Ejercicio 1.3
binToNat :: Binario -> Int
binToNat BaseUno  = 1
binToNat (Cero b) = (2 * binToNat b)
binToNat (Uno b)  = (2 * binToNat (b)) + 1
--binToNat (Uno b)  = 2 **  + binToNat b

--Ejercicio 1.4
sucesor :: Binario -> Binario
sucesor BaseUno = (Cero (BaseUno))
sucesor (Cero b) = (Uno b)
sucesor (Uno b) = (Cero (sucesor b ))

--Ejercicio 1.5
bitsEncendidos :: Binario -> Int
bitsEncendidos BaseUno = 1
bitsEncendidos (Cero b) = bitsEncendidos b
bitsEncendidos (Uno b)  = 1 + bitsEncendidos b


--Ejercicio 2.1
binarios :: [Int] -> [Binario]
binarios [] = []
binarios (x:xs) = [natToBin x] ++ binarios xs

--Ejercicio 2.2
pares :: [Binario] -> [Binario]
pares l = filter (\x ->(mod(binToNat x) 2 ) == 0) l
