module Practica3 where
import Binario


--Ejercicio 2.1
binarios :: [Int] -> [Binario]
binarios [] = []
binarios (x:xs) = [natToBin x] ++ binarios xs

--Ejercicio 2.2
pares :: [Binario] -> [Binario]
pares l = filter (\x ->(mod(binToNat x) 2 ) == 0) l
