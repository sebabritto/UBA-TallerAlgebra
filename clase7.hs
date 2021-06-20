type Set a = [a]

{-|devuelve una lista vacia-}
vacio :: Set Int
vacio = []

{-|pertenece, verifica si un elemento pertenece a una lista-}
pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y: ys) | x == y = True
                    | otherwise = pertenece x ys

eliminar :: Int -> Set Int -> Set Int
eliminar n l | l == [] = []
             | n == head l = eliminar n (tail l)
             | otherwise = head l : eliminar n (tail l)

{-|agregar, agrega un elemento a una lista, si este no pertenece
a esta-}
agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x : c

{-|verifica si un elemento esta incluido en la lista-}
incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

{-|verifica si dos listas son iguales-}
iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

{-| Dado dos conjuntos, devuelve la union entre ellos-}
union :: Set Int -> Set Int -> Set Int
union [] l = l
union (x:xs) l | pertenece x l = union xs l
               | otherwise = x : (union xs l)

{-| Dado dos conjuntos, devuelve la interseccion entre ellos-}
interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) l | pertenece x l = x : (interseccion xs l)
                      | otherwise = interseccion xs l
        
{-|Dado dos conjutnos A y B, devuelve A - B-}
diferencia :: Set Int -> Set Int -> Set Int
diferencia (x:xs) [] = (x:xs)
diferencia [] _ = []
diferencia (x:xs) (y:ys) | pertenece x (y:ys) = diferencia xs (y:ys)
                         | otherwise = x : (diferencia xs (y:ys))

{-|Dado los conjuntos A y B, devuelve la diferencia simetrica-}
diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica l1 [] = l1
diferenciaSimetrica [] l2 = l2
diferenciaSimetrica l1 l2 = union (diferencia l1 l2) (diferencia l2 l1)

{-|Genera el conjunto de partes de un conjunto dado.-}
partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

{-|Agrega un conjunto a un subconjunto-}
agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC l c | perteneceC l c = c
             | otherwise = l:c

{-union de conjuntos de conjuntos-}
unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c       = c
unionC (xs:xss) c | perteneceC xs c = unionC xss c
                  | otherwise = xs : (unionC xss c)

{-|Verifica si un conjunto pertenece a algun subconjunto -}
perteneceC :: Set Int -> Set(Set Int) -> Bool
perteneceC _ [] = False
perteneceC l (cs:css) = iguales l cs || perteneceC l css

{-|Agrega a todos los subconjuntos un elemento, y devuelve este-}
agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos _ [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

{-| genera los subconjuntos del conjunto {1....n}-}
partesN :: Int -> Set(Set Int)
partesN 0 = [[]]
partesN n = (partesN (n-1)) ++ (agregarATodos (n) (partesN(n-1)))

{-| dados dos conjuntos genere todos los pares posibles (como pares de dos elementos)
tomando el primer elemento del primerconjunto y el segundo elemento del segundo
conjunto.-}
productoCartesiano :: Set Int -> Set Int -> Set(Int, Int)
productoCartesiano _ [] = []
productoCartesiano [] _ = []
productoCartesiano (x:xs) (y:ys) =  productoCartesianoAux x (y:ys) ++ (productoCartesiano xs (y:ys))

productoCartesianoAux :: Int -> Set Int -> Set(Int, Int)
productoCartesianoAux _ [] = []
productoCartesianoAux x (y:ys) = (x,y) : productoCartesianoAux x ys