module Main where

  import Data.Char
  import Jugar
  import Peliculas
  contador = 1

  main :: IO()
  main = do
    while(True)

  while :: Bool -> IO()
  while True = func
  while False =   putStrLn ("Has salido del juego")

  -- func es el menú principal de todo el juego
  func :: IO()
  func = do
      putStrLn ("\n" ++"Bienvenido, elije alguna de las opciones: ")
      putStrLn ("[1] Jugar \129513")
      putStrLn ("[2] Instrucciones 📜")
      putStrLn ("[3] Salir ❎")
      putStrLn ("Tu puntaje es de: " ++ show(contador) ++"\n")
      opcion <- getLine
      if(opcion == "1") then
        Jugar.juego
      else if(opcion == "2") then putStrLn(Peliculas.instrucciones)
      else if(opcion /= "3") then putStrLn("Ingresa una opción válida, dushback!" ++ "\n") else putStrLn("\n")
      if(opcion == "3")  then  while(False) else while(True)
