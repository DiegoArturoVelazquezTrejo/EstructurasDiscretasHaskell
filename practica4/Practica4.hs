{-------------------------------------------------------------------------------
-- Practica3.hs - v1.0                                                        --
--                                                                            --
--                       Lógica Computacional, 2019-2                         --
--                     Facultad de Ciencias, UNAM, CDMX                       --
--                                                                            --
-------------------------------------------------------------------------------}

-- :l /home/diego/Documents/github/EstructurasDiscretasPracticas/practica4/Practica4.hs
-- (Hoja 1) o esBalanceado (NodoB (HojaB 3)(HojaB 4))
{- Práctica 3: Definición de tipos de datos -}

module Practica3 where

    data ArbolB a = HojaB a | Void | NodoB (ArbolB a) a (ArbolB a) deriving (Show, Eq, Ord)

    -- Función que obtiene el número de hojas de un árbol.
    numeroHojas :: ArbolB a -> Int
    numeroHojas Void = 0
    numeroHojas  (HojaB _) = 1
    numeroHojas (NodoB (b) a (c)) = numeroHojas b + numeroHojas c

    -- Función que indica si un árbol es balanceado.
    esBalanceado :: ArbolB a -> Bool
    esBalanceado Void = True
    esBalanceado (HojaB _) = True
    esBalanceado (NodoB (b) a (c)) = if((numeroHojas b - numeroHojas c) <= 1 && (numeroHojas b - numeroHojas c)>= -1) then (esBalanceado b && esBalanceado c)
                               else False

    -- Función que aplana un árbol.
    aplana :: ArbolB a -> [a]
    aplana Void = []
    aplana (HojaB n) = [n]
    aplana (NodoB (b) (z) (c)) = (aplana b)++ [z] ++ (aplana c)

    --Moverte a la derecha
    maximo::(Ord a)=>ArbolB a -> a
    maximo (HojaB b) = b
    maximo (Void) = error "El arbol no contiene elementos"
    maximo (NodoB (izq) a (der)) = maximo (der)

    --Moverte a la izquierda
    minimo::(Ord a)=>ArbolB a -> a
    minimo (HojaB b) = b
    minimo (Void) = error "El arbol no contiene elementos"
    minimo (NodoB (izq) a (der)) = minimo (izq)

    --Primero tienes que ver si tu elemento es mayor o menor para saber si irte a la izquierda o a la derecha
    busca ::(Ord a)=>ArbolB a -> a -> Bool
    busca (Void) c = error "El arbol está vacio"
    busca (HojaB b) c = if(b == c) then True else False
    busca (NodoB (izq) a (der)) c = if(a == c) then True
                                    else if(c > a) then busca (der) c else busca(izq) c

    --Como se hacia con una lista
    mapArbol :: ArbolB a -> (a->b) ->ArbolB b
    mapArbol (Void) f = error "El arbol está vacio"
    mapArbol (HojaB a) f = HojaB (f a)
    mapArbol (NodoB (izq) a (der)) f = (NodoB (mapArbol(izq) f) (f a) (mapArbol(der) f))

    --AL agregar, el elemento termina en la hoja, se tiene que comparar
    agrega::(Ord a)=>ArbolB a -> a -> ArbolB a
    agrega (Void) c = HojaB c
    agrega (HojaB n) c = if(c<n) then (NodoB (HojaB c) (n) (Void))
                         else (NodoB (Void) (n) (HojaB c) )
    agrega (NodoB (izq) a (der)) c = if(c<a) then (NodoB (agrega(izq) c) a (der))
                                     else (NodoB (izq) a (agrega(der) c))

    elimina::(Ord a)=>ArbolB a -> a -> ArbolB a
    elimina (Void) c = (Void)
    elimina (HojaB a) c = if(c == a) then (Void) else (HojaB a)
    elimina (NodoB (izq) a (der)) c = if(busca(NodoB (izq) a (der)) c) then (eliminaAuxiliar (NodoB (izq) a (der)) c)
                                      else (NodoB (izq) a (der))

    eliminaAuxiliar :: (Ord a)=>ArbolB a -> a -> ArbolB a
    eliminaAuxiliar (HojaB a) c = if(a == c) then (Void) else (HojaB a)
    eliminaAuxiliar (NodoB (izq) a (der)) c = if( a == c ) then (NodoB (izq) (raiz(der)) (eliminaAuxiliar(integrar (der) a) c))
                                              else if( c < a) then (NodoB (eliminaAuxiliar(izq) c) a (der))
                                                   else (NodoB (izq) a (eliminaAuxiliar (der) c))

    raiz :: (Ord a)=> ArbolB a -> a
    raiz (Void) = error "No tiene elementos"
    raiz (HojaB a) = a
    raiz (NodoB (izq) a (der)) = a

    integrar ::(Ord a)=> ArbolB a -> a -> ArbolB a
    integrar (Void) c = (Void)
    integrar (HojaB a) c = (HojaB c)
    integrar (NodoB (izq) a (der)) c = (NodoB (izq) c (der))

--- Para definir si un arbol es balanceado se tiene que cumplir esta definicion
--    b = #hojas (sub izq) - #hojas (sub der)
--   -1 <= b <= 1


--Pasos para eliminar:
--1. Buscar
--2. Intercambiar con mínimo de subarbol derecho o minimo de subarbol izquierdo
--3. eliminar intercambiado
