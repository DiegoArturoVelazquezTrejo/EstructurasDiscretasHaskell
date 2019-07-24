-- ruta  :l /home/diego/Documents/github/EstructurasDiscretasPracticas/HaskellProjects/redNeuronal.hs


-- Operaciones con matrices

zipFila :: (Num a)=>(a->a->a)->[a]->[a]->[a]
zipFila f [] [] = []nodo.siguiente = nuevoNodo;
        nuevoNodo.anterior = nodo;
        nodo.siguiente.siguiente.anterior = nuevoNodo;
        nuevoNodo.siguiente = nodo.siguiente.siguiente;

        
zipFila f (x:xs) (y:ys) = (f x y):zipFila f xs ys

zipMatriz :: (Num a)=>(a->a->a)->[[a]]->[[a]]->[[a]]
zipMatriz f [] [] = []
zipMatriz f (x:xs) (y:ys) = zipFila f x y :zipMatriz f xs ys

sumaMatriz, restaMatriz, mulMatriz::(Num a)=>[[a]]->[[a]]->[[a]]
sumaMatriz a b = zipMatriz (+) a b
restaMatriz a b = zipMatriz (-) a b
mulMatriz a b = zipMatriz (*) a b

mapeo ::(a->b)->[a]->[b]
mapeo f [] = []
mapeo f (x:xs) = f x : mapeo f xs

-- Matriz que tendrá las entradas
type Input = [[Float]]
-- Matriz que tendrá los labels
type Target = [[Float]]
--Matriz llamada NeuralNetwork que tiene la matriz de pesos y la matriz de bias
type NeuralNetwork = ([[Float]], [[Float]])

simLayer :: Input->NeuralNetwork->(Float, Float)->Target
simLayer pQ (w,b) f = mapeo f (sumaMatriz (mulMatriz w pQ) b)
