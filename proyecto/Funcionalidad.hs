  -- :l /home/diego/Documents/github/EstructurasDiscretasPracticas/proyecto/Funcionalidad.hs
module Funcionalidad where

  import System.Random
  import Data.Char

  -- Lista de posibles artículos que el usuario podría inrgesar junto con el nombre de la película
  articulos = ["the", "this", "these", "those", "that", "ones", "some", "one", "an", "a", "les", "le"]
  -- opcionAleatoria recibe una lista y regresa un elemento aleatoriamente de la lista recibida.

  opcionAleatoria :: [a] -> IO a
  opcionAleatoria l = do
      i <- randomRIO (0, length l - 1) :: IO Int
      return $ l !! i

  -- minusculas recibe una cadena y la tranforma a minusculas
  minusculas str = [toLower cadena | cadena <- str ]

  -- función renderizar (va a quitar espacios en la cadena que haya ingresado el usuario)
  separador :: String -> [String]
  separador s = case dropWhile isSpace s of "" -> []
                                            s' -> w:separador s''
                                                  where (w, s'') = break isSpace s'

  -- Función que quita los articulos de una matriz que contiene a las palabras que ingresó el usuario
  quitarArticulos :: [String] -> [String]
  quitarArticulos (x:xs) = if(elem x articulos) then xs else x:xs

  -- función comprobar (comprobará la cadena que ingresó el usuario con el nombre de la película)
  comparar :: String -> String -> Bool
  comparar a b = (a == b)

  -- función que convierte una lista a una string
  listaAString :: [String] -> String
  listaAString [] = ""
  listaAString (x:xs) = if(length xs > 0 ) then x ++ " " ++ listaAString xs else x ++ listaAString xs


  getName :: (a, a) -> a
  getName tupla = fst tupla

  getEmoji :: (a, a) -> a
  getEmoji tupla = snd tupla
