-- f x y = x * x + y * y

g x y z = x + y + z * z

--Programacion funcional

doble x = 2*x

suma x y = x+y

norma x1 x2 = sqrt (x1^2 + x2^2)

funCon8 x = 8

--Definiciones de funciones por casos

-- f n | n == 0 = 1
--     | n /= 0 = 0
{-
f n | n == 0 = 1
    | otherwise = 0
-}
--La funcion signo
{-
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1
-}
{-signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1
-}

-- La funcion máximo
{-maximo x y | x >= y = x
           | otherwise = y
-}
-- ¿Que hacen las siguientes funciones?

f1 n | n >= 3 = 5 -- No encuentra la definicion de num menores de 3

f2 n | n >= 3 = 5 -- El n = 2 es una excepcion
     | n <= 1 = 8

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

f4 n | n >= 3 = 5 -- si cumple esta condicion no le hace caso a las demas
     | n <= 9 = 7

f5 n | n <= 9 = 7 -- si cumple esta condicion ignora las demas
     | n >= 3 = 5

-- pattern matching

{-f n | n == 0 = 1   -- si inserto 0 se ejecuta esta linea e ignora las demas
    | n /= 0 = 0
-}
-- tambien se puede escribir
f 0 = 1
f n = 0

-- otra posibilidad usando pattern matching
{-
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1-}
-- tambien se puede hacer
signo n | n > 0 = 1
        | otherwise = -1

{-Otro ejemplo
Implementar la funcion cantDeSol, que dados dos numeros b y c,
calcula la cant de soluciones reales de la ecuacion cuadratica
X^2 + bX + c = 0 -}

{-cantDeSol b c | b^2 - 4*c > 0 = 2
              | b^2 - 4*c == 0 = 1
              | otherwise = 0-}

--Otra formae

cantDeSol b c | d > 0 = 2
              | d == 0 = 1
              | otherwise = 0
               where d = b^2 - 4*c

--Tipos de datos
maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

maxRac :: Float -> Float -> Float
maxRac x y | x >= y = x
           | otherwise = y

esMayorA9 :: Int -> Bool
esMayorA9 n | n > 9 = True
            | otherwise = False

esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not (esPar n)

--Otros ejemplos mas raros
{-
funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = (x >= y) || z-}

--Con pattern matching
{-
funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y True = True
funcionRara x y False = x >= y
-}

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y

--Backsticks

pluff :: Int -> Int -> Int
pluff a b = a * b + a + b + 1

--Operador prefijo
-- pluff 12 84

--Operador infijo
-- 12 `pluff` 84 

-- (+) 3 5 = 3 + 5

--Devolver mas de un valor
{-
ejemplo :: Int -> Int -> (Int, Int)
ejemplo a b = ((a `div` b) ^ 2, a - b)

muchos :: Int -> Int -> (Int, Int, Int, Int)
muchos x y = (x * y, x + y, x `mod` y, x `div` y)
-}