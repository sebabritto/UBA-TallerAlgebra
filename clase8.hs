type Set a = [a]

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

combinatorio :: Int -> Int -> Int
combinatorio n k = (fact n) `div` ((fact k) *(fact(n-k)))

combinatorio' :: Int -> Int -> Int
combinatorio' n k | k == 0 = 1
                  | n == k = 1
                  | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | pertenece n c = c
            | otherwise = n : c

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ [] = False
pertenece n (c:cs) = n == c || pertenece n cs

union :: Eq a => Set a -> Set a -> Set a
union [] ys = ys
union (x:xs) ys | pertenece x ys = union xs ys
                | otherwise = agregar x (union xs ys)

{-Agrega cada elemento de una lista a cada lista de listas-}
agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = union (agregarElementoAdelante x c) (agregarElementosAListas xs c)

{-|agrega un elemento a cada lista de listas-}
agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante _ [] = []
agregarElementoAdelante n (cs:css) =  agregar (n:cs) (agregarElementoAdelante n css)

{-|dado un conjunto c y una longitud k genera todas las posibles listas de longitud
k a partir de elementos de c-}
variaciones :: Set Int -> Int -> Set [Int]
variaciones _ 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))

longitud :: Set Int -> Int
longitud [] = 0
longitud l = 1 + longitud (tail l)

{-|dados una lista l, un numero n y una posicion i (contando desde 1) devuelva 
una lista en donde se inserto n en la posicion i de ly los elementos siguientes
corridos en una posicion.-}
insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn ls n i | i == 1 = n : ls
                  | otherwise = head(ls) : (insertarEn (tail ls) n (i-1))

{-|dado un conjunto de enteros, genere las posibles permutaciones de los numeros del conjunto pasado por parametros-}
permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c

insertarEnCadaPos :: [Int] -> Int -> Int -> Set[Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] _ = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs + 1)) `union`
                                               (insertarEnCadaPosDeTodasLasListas xss c)
---------------------------------------------Ejercicios----------------------------------
{-|Todas las formas de ubicar n bolitas numeradas en k cajas-}

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas _ 0 = [[]]
bolitasEnCajas n k = variaciones (generaListaHasta k) n

generaListaHasta :: Int -> Set Int
generaListaHasta 1 = [1]
generaListaHasta n = generaListaHasta(n-1) ++ [n]

-----------------------------------------------------------------------------------------
{-|Todas las formas de ubicar n bolitas numeradas en k cajas tal que la primera caja nunca este vacia-}
primeraCaja :: Int -> Int -> Set [Int]
primeraCaja _ 0 = [[]]
primeraCaja n k = diferencia (variaciones (generaListaHasta k) n) (variaciones (tail (generaListaHasta k)) n)

{-|Dado dos conjutnos A y B, devuelve A - B-}
diferencia :: Set [Int] -> Set [Int] -> Set [Int]
diferencia (xs:xss) [] = (xs:xss)
diferencia [] _ = []
diferencia (xs:xss) (ys:yss) | pertenece xs (ys:yss) = diferencia xss (ys:yss)
                             | otherwise = xs : (diferencia xss (ys:yss))

-----------------------------------------------------------------------------------------
{-|Todas las listas ordenadas de k numeros distintos tomados del conjunto {1,...n}-}
ordenadoDistinto :: Int -> Int -> Set [Int]
ordenadoDistinto _ 0 = [[]]
ordenadoDistinto n k = eliminarRepetidos(ordenarListas(eliminarIguales(bolitasEnCajas n k)))

{-|Ordena la lista de listas-}
ordenarListas :: Set [Int] -> Set[Int]
ordenarListas [] = []
ordenarListas (xs:xss) = ordenarElementos(xs) : ordenarListas(xss)

{-|ordenar ordena los elementos de forma creciente-}
ordenarElementos :: [Int] -> [Int]
ordenarElementos l | tail l == [] = l
                   | otherwise = minimo l : ordenarElementos (quitar (minimo l) l)

{-|quitar elimina la primera aparicion del elemento en la lista (de haberla)-}
quitar :: Int -> [Int] -> [Int]
quitar n l | n == head l = tail l
           | tail l == [] = []
           | otherwise = head l : quitar n (tail l)
{-|minimo me devuelve el valor minimo-}
minimo :: [Int] -> Int
minimo l | tail l == [] = head l
         | head l < minimo (tail l) = head l
         | otherwise = minimo (tail l)

{-|Elimina las repeticiones de listas, dejando una sola-}
eliminarRepetidos :: Set [Int] -> Set[Int]
eliminarRepetidos (xs:xss) | xss == [] = [xs]
                           | pertenece xs xss = eliminarRepetidos(xss)
                           | otherwise = xs : eliminarRepetidos(xss)

{-|Elimina las listas cuyos elementos son iguales-}
eliminarIguales :: Set[Int] -> Set [Int]
eliminarIguales [] = []
eliminarIguales (xs:xss) | iguales xs = eliminarIguales(xss)
                         | otherwise = xs : eliminarIguales(xss)

{-|Si todos los elementos de una lista son iguales devuelve True-}
iguales :: Set Int -> Bool
iguales (x:xs) | xs == [] = True
               | otherwise = x == (head xs) && iguales(xs)

---------------------------------------------------------------------------------------
{-|Todas las sucesiones de los caracteres 'a' y 'b' de longitud n y m respectivamente-}
sucesionesDeCaracteres :: Int -> Int -> Set[Char]
sucesionesDeCaracteres n k = permutacionesCaracteres(generacionDeOracionConDos n k)

generacionDeOracionConDos :: Int -> Int -> Set Char
generacionDeOracionConDos n k = (generacionDeA n) ++ (generacionDeB k)

generacionDeA :: Int -> Set Char
generacionDeA 0 = []
generacionDeA n = 'a' : generacionDeA(n-1)

generacionDeB:: Int -> Set Char
generacionDeB 0 = []
generacionDeB m = 'b' : generacionDeB(m-1)

permutacionesCaracteres :: Set Char -> Set[Char]
permutacionesCaracteres [] = [[]]
permutacionesCaracteres (c:cs) = insertarEnCadaPosDeTodasLasListasCar (permutacionesCaracteres cs) c

insertarEnCadaPosDeTodasLasListasCar :: Set [Char] -> Char -> Set [Char]
insertarEnCadaPosDeTodasLasListasCar [] _ = []
insertarEnCadaPosDeTodasLasListasCar (xs:xss) c = (insertarEnCadaPosCar xs c (length xs + 1)) `union`
                                               (insertarEnCadaPosDeTodasLasListasCar xss c)

insertarEnCadaPosCar :: [Char] -> Char -> Int -> Set[Char]
insertarEnCadaPosCar xs c 1 = agregar (insertarEnCar xs c 1) vacio
insertarEnCadaPosCar xs c i = agregar (insertarEnCar xs c i) (insertarEnCadaPosCar xs c (i-1))

insertarEnCar :: [Char] -> Char -> Int -> [Char]
insertarEnCar ls n i | i == 1 = n : ls
                  | otherwise = head(ls) : (insertarEnCar (tail ls) n (i-1))

----------------------------------------------------------------------------------------

{-|Todas las sucesiones de 'a', 'b' y 'c' de longitud n, m y k-}
sucesionesDeCaracteresConTres :: Int -> Int -> Int -> Set[Char]
sucesionesDeCaracteresConTres n m k = permutacionesCaracteres(generacionDeOracionConTres n m k)

generacionDeC :: Int -> Set Char
generacionDeC 0 = []
generacionDeC k = 'c' : generacionDeC(k-1)

generacionDeOracionConTres :: Int -> Int -> Int -> Set Char
generacionDeOracionConTres n m k = generacionDeA(n) ++ generacionDeB(m) ++ generacionDeC(k)

--------------------------------------------------------------------------------------

{-|Dados un conjunto de enteros y un entero k, genera todos los subconjuntos de k
elementos del conjunto pasado por paramentro-}

subconjuntos :: Set Int -> Int -> Set (Set Int)
subconjuntos _ 0 = []
subconjuntos [] _ = []
subconjuntos l k = ordenadoDistinto k (darUltimo l)

darUltimo :: Set Int -> Int
darUltimo [] = 0
darUltimo l | tail l == [] = head l
            | otherwise = darUltimo (tail l)