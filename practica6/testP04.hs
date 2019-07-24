import Prop

data Test = P Bool

instance Show Test where
 show (P True) = "CORRECTO"
 show (P False) = "ERROR"

main = do
 print "---------- Pruebas del Ejercicio 1.1; Función: eliminacion ----------"
 print (P ((eliminacion (Impl (Var "P") (Var "Q"))) == (Disy (Neg (Var "P")) (Var "Q"))))
 print (P ((eliminacion (Impl (Impl (Var "P") (Var "Q")) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero)))) == (Disy (Neg (Disy (Neg (Var "P")) (Var "Q"))) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero)))))
 print (P ((eliminacion (Neg (Conj (Impl (Var "P") (Var "Q")) (Syss (Var "Q") (Disy (Var "R") (Var "S")))))) == (Neg (Conj (Disy (Neg (Var "P")) (Var "Q")) (Conj (Disy (Neg (Var "Q")) (Disy (Var "R") (Var "S"))) (Disy (Neg (Disy (Var "R") (Var "S"))) (Var "Q")))))))
 print (P ((eliminacion (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q"))) == (Disy (Neg (Conj (Disy (Neg (Var "P")) (Var "Q")) (Var "P"))) (Var "Q"))))
 print (P ((eliminacion (Syss (Disy (Var "P") (Var "Q")) (Neg (Disy (Var "P") (Var "Q"))))) == (Conj (Disy (Neg (Disy (Var "P") (Var "Q"))) (Neg (Disy (Var "P") (Var "Q")))) (Disy (Neg (Neg (Disy (Var "P") (Var "Q")))) (Disy (Var "P") (Var "Q"))))))

 print "---------- Pruebas del Ejercicio 1.2; Función: deMorgan ----------"
 print (P ((deMorgan (Impl (Var "P") (Var "Q"))) == (Impl (Var "P") (Var "Q"))))
 print (P ((deMorgan (Impl (Impl (Var "P") (Var "Q")) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero)))) == (Impl (Impl (Var "P") (Var "Q")) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero)))))
 print (P ((deMorgan (Neg (Conj (Impl (Var "P") (Var "Q")) (Syss (Var "Q") (Disy (Var "R") (Var "S")))))) == (Disy (Neg (Impl (Var "P") (Var "Q"))) (Neg (Syss (Var "Q") (Disy (Var "R") (Var "S")))))))
 print (P ((deMorgan (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q"))) == (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q"))))
 print (P ((deMorgan (Syss (Disy (Var "P") (Var "Q")) (Neg (Disy (Var "P") (Var "Q"))))) == (Syss (Disy (Var "P") (Var "Q")) (Conj (Neg (Var "P")) (Neg (Var "Q"))))))

 print "---------- Pruebas del Ejercicio 2.1; Función: interp ----------"
 print (P (not $ interp (Impl (Var "P") (Var "Q")) [("P", Verdadero), ("Q", Falso)]))
 print (P (interp (Impl (Impl (Var "P") (Var "Q")) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero))) [("P", Verdadero), ("Q", Verdadero) , ("R", Falso)]))
 print (P (interp (Neg (Conj (Impl (Var "P") (Var "Q")) (Syss (Var "Q") (Disy (Var "R") (Var "S"))))) [("P", Verdadero), ("Q", Falso), ("R", Falso), ("S", Verdadero)]))
 print (P (interp (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q")) [("P", Falso), ("Q", Falso)]))
 print (P (not $ interp (Syss (Disy (Var "P") (Var "Q")) (Neg (Disy (Var "P") (Var "Q")))) [("P", Verdadero), ("Q", Verdadero)]))

 print "---------- Pruebas del Ejercicio 2.2; Función: truthTable ----------"
 print (P ((truthTable (Impl (Var "P") (Var "Q"))) == "Contingencia"))
 print (P ((truthTable (Impl (Impl (Var "P") (Var "Q")) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero)))) == "Contingencia"))
 print (P ((truthTable (Neg (Conj (Impl (Var "P") (Var "Q")) (Syss (Var "Q") (Disy (Var "R") (Var "S")))))) == "Contingencia"))
 print (P ((truthTable (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q"))) == "Tautología"))
 print (P ((truthTable (Conj (Disy (Var "P") (Var "Q")) (Neg (Disy (Var "P") (Var "Q"))))) == "Contradicción"))

 print "---------- Pruebas del Ejercicio 2.3; Función: correcto ----------"
 print (P (correcto [(Impl (Var "P") (Var "Q")), (Var "P")] (Var "Q")))
 print (P (correcto [(Impl (Impl (Var "P") (Var "Q")) (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero))), (Neg (Var "P"))] (Conj (Neg (Var "R")) (Disy (Var "P") Verdadero))))
 print (P (not $ correcto [(Var "P"), (Disy (Var "Q") (Var "P"))] (Neg (Conj (Impl (Var "P") (Var "Q")) (Syss (Var "Q") (Disy (Var "R") (Var "S")))))))
 print (P (correcto [] (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q"))))
 print (P (correcto [(Syss (Disy (Var "P") (Var "Q")) (Neg (Disy (Var "P") (Var "Q"))))] (Syss (Disy (Var "P") (Var "Q")) (Neg (Disy (Var "P") (Var "Q"))))))
