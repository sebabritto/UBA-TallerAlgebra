--Calcular el valor absoluto de un numero
absoluto :: Int -> Int
absoluto n | n < 0 = -n
           | otherwise = n

--Devuelve el maximo entre el valor absoluto de dos numeros
maxAbs :: Int -> Int -> Int
maxAbs x y | absoluto x >= absoluto y = x
           | otherwise = y

--Devuelve el maximo entre tres numeros enteros
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z

{-Dados dos numeros racionales, decide si alguno de los dos es igual 
a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching)-}
mens :: Bool -> String
mens s | s == True = "Uno de los valores ingresados es 0"
       | otherwise = "Ninguno es 0"

algunoEs0 :: Float -> Float -> String
{-algunoEs0 x y | x == 0 || y == 0 = True
              | otherwise = False-}
--Con pattern matching
algunoEs0 x y = mens (x == 0 || x == y)

--Dados dos numeros racionales, decide si ambos son iguales a 0
ambosSon0 :: Float -> Float -> Bool
{-ambosSon0 x y | x == 0 && y == 0 =  True
              | otherwise = False-}
--pattern matching
ambosSon0 x y = x == 0 && y == 0

{-Dados dos numeros naturales, decidir si el primero es multiplo del 
segundo-}
esMultiploDe:: Int -> Int -> Bool
esMultiploDe x y | x < 0 || y < 0 = undefined
                 | x `mod` y == 0 = True
              --   | y `mod` x == 0 = True
                 | otherwise = False

--Dado un numero natural, extraer su digito de las unidades.
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

--Dado un numero natural, extraer su digito de las decenas

digitoDecenas :: Int -> Int
digitoDecenas x | x >= 0 && x < 10 = x
                | x >= 0 = div (mod x 100) 10
                | otherwise = undefined