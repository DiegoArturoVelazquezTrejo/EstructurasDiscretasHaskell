module Prueba where

  import System.Random
  --import Data.Char

  randomA :: g -> (Int, g)
  randomA g = next g

  numero :: Int -> Int
  numero i = i
