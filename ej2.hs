-- Ejercicios de la clase 02

estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | (x <= 3) && (y <= 3) = True
                      | (x > 3) && (x <= 7) && (y > 3) && (y <= 7) = True
                      | (x > 7) && (y > 7) = True
                      | otherwise = False

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt(((x1 - x2)**2) + ((y1 - y2)**2))

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | (mod x 2 == 0) = 1
                         | (mod y 2 == 0) = 2
                         | (mod z 2 == 0) = 3
                         | otherwise = 4

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)
