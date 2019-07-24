--ESTRUCTURAS DISCRETAS 2019-2
--Práctica 2

-- :l /home/diego/Documents/github/EstructurasDiscretasPracticas/practica2/Practica2.hs

module Practica2 where

--DEFINICIÓN DE LISTAS

--1.1: Naturales.
nat = [1 .. ]
--1.2: Multiplos de diez.
multiplosDiez = [ x  * 10 | x <- nat]
--1.3: potencias de 2.
potenciasDos = [ x ** 2 | x <- nat]
--1.4: Números pares.
pares = [2 * x | x <- nat]
--1.5: años desde el año de tu nacimiento.
anosVividos = [2000 .. 2019]


--DEFINICIÓN DE FUNCIONES

--Ejercicio 2.1:
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci (n - 2)

--Ejercicio 2.2:
elemento :: ( Eq a ) => [a] -> a -> Bool
elemento [] n = False
elemento (x:xs) n = if x == n then True
                    else elemento (xs) n

--Ejercicio 2.3:
sumaLista ::(Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

--Ejercicio 2.4:
meses :: [Int] -> [String]
--Tu código va aquí
meses [] = []
meses (x:xs) = ["Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre",
                "Diciembre"] !!(x - 1) : meses xs

--Ejercicio 2.5:
divisoresPropios :: Int -> [ Int ]
divisoresPropios x = divisoresAuxiliar x (x-1)


divisoresAuxiliar :: Int -> Int -> [Int]
divisoresAuxiliar 0 0 = [0]
divisoresAuxiliar n 0 = []
divisoresAuxiliar n m = if(mod n m == 0) then m: divisoresAuxiliar n (m-1)
                        else divisoresAuxiliar n (m-1)


--Ejercicio 2.6:
esPerfecto :: Int -> Bool
esPerfecto x = if( sumaLista(divisoresPropios x) == x) then True
               else False

--Ejercicio 2.7:
sonAmigos :: Int -> Int -> Bool
sonAmigos x y = if( sumaLista(divisoresPropios x) == y && sumaLista(divisoresPropios y) == x) then True
                else False

--Ejercicio 2.8:
supersuma :: Int -> Int
supersuma n = if(n < 10) then n
              else mod n 10 + supersuma (div n 10)


--Ejercicio 2.9:
numeros = ["rei","ichi","ni","san","yon","go","roku","nana","hachi","kyu","ju"]
japones :: Int -> String
--Tu código va aquí
japones x = if(x <= 10) then numeros !! x
            else numeros !! (div x 10) ++ " ju " ++ if((mod x 10) > 0 ) then numeros !! (mod x 10) else " "

-- Métodos auxiliares
-- Metodo para obtener la cabeza de una lista
car :: [a] -> a
car [] = error "No hay elementos"
car (x:xs) = x

-- Método para obtener la cola de una lista
cdr :: [a] -> [a]
cdr [] = error "No hay elementos"
cdr (x:xs) = xs

-- Método para quitar n elementos de una lista
toma :: [a] -> Int -> [a]
toma [] n = []
toma (x:xs) 0 = (x:xs)
toma (x:xs) n  = toma xs (n-1)


-- para los numeros naturales basta con poner [1..]
-- para definir rangos tenemos que [1 .. 13]
-- y para buscar basta con poner el numero [1 .. 13] 15
--
-- Para definir a la lista de los pares tenemos que:
-- nat = [1 .. ]
-- pares = [ 2 * x | x <- nat]

mapeo ::(a->b)->[a]->[b]
mapeo f [] = []
mapeo f (x:xs) = f x : mapeo f xs

-- Lambdas (funciones anonimas)     asi quda la funcion anonimca   (\x -> x + 3)
--  mapeo (\x -> x + 3) [1 .. 10]
filtraElem :: (Eq a) => a ->[a]->[a]
filtraElem k l = filter (\x -> (x == k)) l

filtraLongitud :: Int -> [String] -> [String]
filtraLongitud n l = filter (\x -> length x <= n ) l
--binario :: Int -> [Int]
--binario 1 = [1]
--binario n = mod n 2 : binario (n / 2)
filtraPares :: [Int] -> [Int]
filtraPares l = filter (\x -> (mod x 2) == 0) l

mapFibonacci :: [Int] -> [Int]
mapFibonacci (x:xs) = mapeo fibonacci (x:xs)

filtra ::(Int->Bool)->[Int]->[Int]
filtra f [] = []
filtra f (x:xs) = if(f x == True) then x: filtra f xs
                  else filtra f xs


mapFibonacci2 :: Int -> [Int]
mapFibonacci2 n = map fibonacci [0 .. n]

filtraElemMenores :: Int -> [Int]->[Int]
filtraElemMenores k l = filter (\x -> (x < k)) l

creaNat :: Int -> [Int]
creaNat 0 = [0]
creaNat n = creaNat(n - 1)++[n]


divisores :: Int -> [Int]
divisores 0 = []
divisores n = (auxiliar 1 n) ++ [n]

auxiliar :: Int -> Int -> [Int]
auxiliar m n = if(m > div n 2) then []
               else if(mod n m == 0) then m:auxiliar (m + 1) n
                    else auxiliar (m+1) n


inserta :: Int -> [Int] -> [Int]
inserta n [] = [n]
inserta n (x:xs) = if(n<x) then n:(x:xs)
                   else x:inserta n xs
