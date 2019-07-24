-- :l /home/diego/Documents/github/EstructurasDiscretasPracticas/proyecto/Jugar.hs
module Jugar where

  import System.Random
  import Data.Char
  import Peliculas
  import Funcionalidad


  -- pelicula es la variable que amacenará la opción aleatoria de la lista de películas
  peliculaRandom = (opcionAleatoria(Peliculas.movies))
  emojis = "\127932 \128116 \128102"
  --emojis = getEmoji peliculaRandom
  nombreDePelicula = minusculas "Whiplash"
  --nombreDePelicula = getName peliculasRandom

  juego :: IO()
  juego = do
      foo <- putStrLn "¿Cuál es el nombre de la película?"
      foo <- putStrLn emojis
      nombre <- getLine
      let finalUsuario = listaAString(quitarArticulos(separador (minusculas nombre)))
      let respuesta = comparar finalUsuario nombreDePelicula
      if(respuesta)
            then
                putStrLn("✅")
            else
                putStrLn("❌")
