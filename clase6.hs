--Listas

{-|Devuelve el primer elemento de la lista}
head :: [a] -> a
-}

{-Devuelve la cola de la lista, es decir, la lista sin el 
1er elemento
tail :: [a] -> [a]
-}

{-Agrega un elemento
(:) :: a -> [a] -> [a]}
-}

sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l)

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

longitudPM :: [Int] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitud xs

pertenecePM :: Int -> [Int] -> Bool
pertenecePM _ [] = False
pertenecePM l (x:xs) = l == x || pertenecePM l (xs)

{-|Productoria: devuelve la productoria de los elementos-}
productoria :: [Int] -> Int
productoria l | l == [] = 1
              | otherwise = head l * productoria(tail l)

{-|sumarN dado un numero N y una lista xs, suma N a cada elemento de xs-}

sumarN :: Int -> [Int] -> [Int]
sumarN n l | tail l == [] = [n+(head l)]
           | otherwise = (n+(head l)) : (sumarN n (tail l))

{-|sumarElPrimero, dada una lista no vacia xs suma el primer elemento a cada 
elemento de xs-}
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero l | l == [] = []
                 | otherwise = sumarN (head l) l

{-|sumarElUltimo, dada una lista no vacia xs, suma el ultimo elemento a cada
elemento de xs-}
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l | l == [] = []
                | otherwise = sumarN (darUlt l) l

darUlt :: [Int] -> Int
darUlt l | tail l == [] = head l
         | otherwise = darUlt (tail l)

{-|pares, devuelve una lista con los elementos pares de la lista original-}
pares :: [Int] -> [Int]
pares l | tail l == [] && head l `mod` 2 == 0 = [head l]
        | tail l == [] && head l `mod` 2 /= 0 = []
        | head l `mod` 2 == 0 = head l : pares (tail l)
        | otherwise = pares (tail l)

{-|quitar elimina la primera aparicion del elemento en la lista (de haberla)-}
quitar :: Int -> [Int] -> [Int]
quitar n l | n == head l = tail l
           | tail l == [] = []
           | otherwise = head l : quitar n (tail l)

{-|quitarTodas elimina todas las apariciones del elemento en la lista-}
quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n l | head l == n && tail l == [] = []
                | head l /= n && tail l == [] = l
                | head l /= n = head l : quitarTodas n (tail l)
                | otherwise = quitarTodas n (tail l)

{-|hayRepetidos, indica si una lista tiene elementos repetidos-}
hayRepetidos :: [Int] -> Bool
hayRepetidos l | tail l == [] = False
               | otherwise = pertenece (head l) (tail l) || hayRepetidos (tail l)

{-|eliminarRepetidosAlFinal deja en la lista la primera aparicion de cada elemento,
eliminando las repeticiones adicionales-}
eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal l | tail l == [] = l
                           | hayRepetidos l = head l : eliminarRepetidosAlFinal(quitarTodas (head l) (tail l))
                           | otherwise = head l : eliminarRepetidosAlFinal (tail l)

{-|eliminarRepetidosAlInicio deja en la lista la ultima aparicion de cadaelemento,
eliminando las repeticiones adicionales-}
eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio l | tail l == [] = l
                            | pertenece(head l) (tail l) = eliminarRepetidosAlInicio (tail l)
                            | otherwise = head l : eliminarRepetidosAlFinal(tail l)

{-|maximo calcula el maximo elemento de una lista no vacia-}
maximo :: [Int] -> Int
maximo l | tail l == [] = head l
         | head l > maximo(tail l) = head l
         | otherwise = maximo(tail l)

{-|ordenar ordena los elementos de forma creciente-}
ordenar :: [Int] -> [Int]
ordenar l | tail l == [] = l
          | otherwise = minimo l : ordenar (quitar (minimo l) l)

{-|minimo me devuelve el valor minimo-}
minimo :: [Int] -> Int
minimo l | tail l == [] = head l
         | head l < minimo (tail l) = head l
         | otherwise = minimo (tail l)

{-| reverso dada una lista invierte su orden-}
reverso :: [Int] -> [Int]
reverso l | tail l == [] = l
          | otherwise = darUlt l : reverso (quitarUlt l)

quitarUlt :: [Int] -> [Int]
quitarUlt l | tail l == [] = []
            | otherwise = head l : quitarUlt(tail l)

{-| concatenar devuelve la concatenacion de la primera con la segunda-}
concatenar :: [Int] -> [Int] -> [Int]
concatenar l1 l2 = l1 ++ l2

{-|zipi devuelve una lista de tuplas, cada tupla contiene elementos de ambas listas que 
ocurren en la misma posicion-}
zipi :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)]
zipi l1 l2 | (tail l1 == []) || (tail l2 == []) = [(head l1, head l2)]
           | otherwise = (head l1, head l2) : zipi (tail l1) (tail l2)