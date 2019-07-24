module Prop where

 import Data.List

--DEFINICIONES
-- :l /home/diego/Documents/github/EstructurasDiscretasPracticas/practica6/Prop.hs
-- Tipo de dato para representar las expresiones de la lógica proposicional
 data Prop = Verdadero
           | Falso
           | Var String
           | Neg Prop
           | Conj Prop Prop
           | Disy Prop Prop
           | Impl Prop Prop
           | Syss Prop Prop
           deriving (Eq,Ord)

-- Sinónimo para representar el estado
 type Estado = [(String, Prop)]

--Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
 instance Show Prop where
  show Verdadero = "V" -- V
  show Falso = "F" -- F
  show (Var x) = x -- P
  show (Neg p) = "¬ " ++ show p -- ¬ P
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)


-- EQUIVALENCIAS LÓGICAS

 -- Ejercicio 1.1
 eliminacion :: Prop -> Prop
 eliminacion (Falso) = Falso
 eliminacion (Verdadero) = Verdadero
 eliminacion (Var a) = (Var a)
 eliminacion (Impl p q) = (Disy (Neg (eliminacion p)) (eliminacion q))
 eliminacion (Syss p q) = (Conj (Disy (Neg p) q) (Disy (Neg q) p))
 eliminacion (Conj p q) = (Conj (eliminacion p) (eliminacion q))
 eliminacion (Disy p q) = (Disy (eliminacion p) (eliminacion q))
 eliminacion (Neg p)    = (Neg(eliminacion p))


 -- Ejercicio 1.2
 deMorgan :: Prop -> Prop
 deMorgan (Neg (Conj a b)) = Disy (Neg a) (Neg b)
 deMorgan (Neg (Disy a b)) = Conj (Neg a) (Neg b)
 deMorgan (Neg a) = Neg(deMorgan a)
 deMorgan (Conj p q) = Conj (deMorgan p) (deMorgan q)
 deMorgan (Disy p q) = Disy (deMorgan p) (deMorgan q)
 deMorgan (Syss p q) = Syss (deMorgan p) (deMorgan q)
 deMorgan (Impl p q) = Impl (deMorgan p) (deMorgan q)
 deMorgan a = a


-- EVALUACIÓN Y ANÁLISIS SINTÁCTICO DE EXPRESIONES

 encuentra :: (Ord a) => a -> [(a, b)] -> b
 encuentra a [(b,c)] = if(a == b) then c else error"La variable no se encontró"
 encuentra a [] = error "La variable no se encuentra definida"
 encuentra a (x:xs) = if(a == fst (x)) then snd(x) else encuentra a xs

 -- Ejercicio 2.1
 interp :: Prop -> Estado -> Bool
 interp Falso      estado = False
 interp Verdadero  estado = True
 interp (Var x) s = interp (encuentra x s) s
 interp (Neg x) s = not(interp x s)
 interp (Conj x y) s = (interp x s) && (interp y s)
 interp (Disy x y) s = (interp x s) || (interp y s)
 interp (Impl x y) s = interp (eliminacion(Impl x y)) s
 interp (Syss x y) s = interp (eliminacion(Syss x y)) s





 contarVariables :: Prop -> [String]
 contarVariables (Falso) = []
 contarVariables (Verdadero) = []
 contarVariables (Var a) = [a]
 contarVariables (Disy a b) = (contarVariables a) ++ (contarVariables b)
 contarVariables (Conj a b) = (contarVariables a) ++ (contarVariables b)
 contarVariables (Impl a b) = (contarVariables a) ++ (contarVariables b)
 contarVariables (Syss a b) = (contarVariables a) ++ (contarVariables b)
 contarVariables (Neg a)    = (contarVariables a)

 eliminarep :: (Eq a) => [a] -> [a]
 eliminarep [] = []
 eliminarep (x:xs) = if(elem x xs) then eliminarep xs
                             else x:(eliminarep (xs))

 naturalesEnRangoDeCombinacion :: Int -> [Int]
 naturalesEnRangoDeCombinacion 0 = [0]
 naturalesEnRangoDeCombinacion a = a:naturalesEnRangoDeCombinacion (a - 1)

 contarCombinaciones :: Prop -> [Int]
 contarCombinaciones prop = naturalesEnRangoDeCombinacion((potencia 2 (length (eliminarep (contarVariables prop)))) - 1)

 binario :: Int -> [Int]
 binario 0 = [0]
 binario 1 = [1]
 binario a = (mod a 2): binario (div a 2)

 construirEstado :: [Int] -> [String] -> Estado
 construirEstado [0] [a] = [(a, Falso)]
 construirEstado [1] [a] = [(a, Verdadero)]
 construirEstado []  [a] = [(a, Falso)]
 construirEstado (x:xs) (y:ys) = (construirEstado [x] [y]) ++ (construirEstado xs ys)

 potencia :: Int -> Int -> Int
 potencia x 1 = x
 potencia x a = x * potencia x (a - 1)

 normalizar :: [Int] -> Int -> [Int]
 normalizar (x:xs) a = if((a - length(x:xs)) > 0) then normalizar ((x:xs) ++ [0]) (a)
                       else (x:xs)

  -- Ejercicio 2.2
 truthTableAuxiliar :: Prop -> Int -> Bool
 truthTableAuxiliar prop int = definirEstado prop (eliminarep(contarVariables prop)) int

 truthTableAuxiliar2 :: Prop -> [Int] -> [Bool]
 truthTableAuxiliar2 prop  [] = []
 truthTableAuxiliar2 prop (x:xs) = (truthTableAuxiliar prop x):(truthTableAuxiliar2 prop xs)



 truthTable :: Prop -> String
 truthTable prop =  decision (truthTableAuxiliar2 prop (contarCombinaciones prop))

 decision :: [Bool] -> String
 decision (x:xs) = if(elem True (x:xs) && elem False (x:xs)) then "Contingencia"
                   else if(elem True (x:xs)) then "Tautologia"
                        else "Contradiccion"

 definirEstado :: Prop -> [String] -> Int -> Bool
 definirEstado prop x a = interp prop (construirEstado (normalizar (binario a) (length x)) x)


 -- Ejercicio 2.3
 correcto :: [Prop] -> Prop -> Bool
 correcto = error "Hola"
