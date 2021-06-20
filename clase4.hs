sumatoriaDeGaussRecursion :: Int -> Int
{-| sumatoria de los primeros n naturales con recursión-}
sumatoriaDeGaussRecursion n | n < 0 = undefined 
                            | n == 1 = 1
                            | otherwise = sumatoriaDeGaussRecursion(n-1) + n

{-| sumatoria de los primeros n naturales con la formula-}
sumatoriaDeGaussFormula :: Int -> Int
sumatoriaDeGaussFormula n = (n*(n+1)) `div` 2

{-| sumatoria con i de 0 a n de 2^i-}
f1 :: Int -> Int
f1 n | n < 0 = undefined
     | n == 0 = 2^n
     | otherwise = f1(n-1) + 2^n

{-| i de 1 a n, sumatoria de q^i-}
f2 :: Int -> Float -> Float
f2 n q | n < 0 = undefined
       | n == 0 = 0 --está piola definir el 0 cuando es hasta 1 porque el conjunto vacío simepre suma cero
       | otherwise = f2 (n-1) q + q^n

{-| i de 1 a 2n, sumatoria de q^i-}
f3 :: Int -> Float -> Float
f3 n q | n < 0 = undefined
       | n == 0 = 0 --está piola definir el 0 cuando es hasta 1 porque el conjunto vacío simepre suma cero
       | otherwise = f3 (n-1) q + q^(2*n-1) + q^(2*n) --desarmo la sumatoria (hago aparecer la sumatoria hasta 2(n-1))

f3v2 :: Int -> Float -> Float
f3v2 n q = f2(2*n) q --composición de funciones

{-|sumatoria de n a 2n de q^i-}
f4 :: Int -> Float -> Float
f4 n q | n == 0 = 1
       | otherwise = f4 (n-1) q - q^(n-1) + q^(2*n-1) + q^(2*n) -- abro la sumatoria haciendola ir hasta 2(n-1), el problema es que ahora arranca en n-1, entonces le resto ese elemento

{-|sumatoria de n a 2n de q^i-}
f4v2 :: Int -> Float -> Float
f4v2 n q = f3 n q - f2 (n-1) q --la hago de 1 a 2n y le resto de 1 a n-1

{-|factorial de n-}
fact :: Int -> Int
fact n | n < 0 = undefined
       | n == 0 = 1
       | otherwise = n*(fact (n-1))

{-|aproximación de e con n términos de la sumatoria de 0 a n de 1/(i!)-}
eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (eAprox (n-1)) + 1 / (fromIntegral (fact n))

--Para definir constantes
{-| número e definido con 10 términos de la sumatoria-}
e :: Float
e = eAprox 10

{-| sumatoria de i=1 a i=n de la sumatoria de j=1 a j=m de jî basicamente la suma de todas las combinaciones de i y j-}
f :: Int -> Int -> Int
f 0 m = 0
f n m = (f (n-1) m) + round(f2 m (fromIntegral n)) --el fromintegral es porque f2::Int->Float->Float, el round es porque haskell no sabe sumar int y float y f2(blablabla) es algo coma cero, entonces no perdemos nada

{-| sumatoria de a=1 a b=n de sumatoria de b=1 a b=m de q^(a+b)-}
sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + q^m * (f2 n q)

{-|sumatoria de p=1 a p=n de la sumatoria de q=1 a q=m de p/q-}
sumaRacionales :: Int -> Int -> Float
sumaRacionales n m | m == 0 = 0
                   | otherwise = sumaRacionales n (m-1) + (fromIntegral (sumatoriaDeGaussFormula n)) / (fromIntegral m)


--EJERCICIOS
{-|sumatoria de j=i a j=n de i^j-}
g1 :: Int -> Int -> Float
g1 n i = f2 n (fromIntegral i) - f2 (i-1) (fromIntegral i) --la hago de 1 a n y despúes le resto de 1 a (i-1)




{-sumatoria desde i=1 hasta n de la sumatoria de j=i hasta n de i^j-}
g2 :: Int -> Int 
g2 n | n == 1 = 1
     | otherwise = g2 (n-1) + g2complemento 1 n

{-|sumatoria de i=input1 a n de i^n-}
g2complemento :: Int -> Int -> Int
g2complemento i n | i == n = n^n
                  | otherwise = i^n + g2complemento (i+1) n



{-|sumatoria de i=1 a i=n de 2^n, i tiene que ser par-}
g3 :: Int -> Int
g3 n | n <= 0 = 0 --caso base
     | n `mod` 2 == 0 = 2^n + g3(n-2) --si es par sacamos el término afuera de la sumatoria. Es -2 porque -1 va a ser impar.
     | otherwise = g3(n-1) --si es impar hagámoslo par




{-|dado un n, suma todos los numeros naturales menores o iguales que n que tengan todos los d´ıgitos iguales-}
g4 :: Int -> Int
g4 n | n <= 0 = 0 --caso base
     | digitosIguales n == True = n + g4 (n-1) --si tiene los digitos iguales lo sumamos
     | otherwise = g4 (n-1) --si no tenía los digitos iguales probamos con el que sigue
    where digitosIguales :: Int -> Bool --explicación en clase 3
          digitosIguales n | n < 0 = digitosIguales (-n) --lo hago positivo
                           | n < 10 = True --para el caso especial de que pongan un numero de una sola cifra
                           | n `mod` 10 /= ( (n `mod` 100) `div` 10) = False --si el ultimo digito (mod10) es distinto del anteultimo (mod100 me da los dos ultimos y al hacerle div10 me deja solo el primero de esos dos) ya no se cumple lo pedido -> False
                           | n >= 100 = digitosIguales (n `div` 10) -- si lo de arriba es true hagamoslo de vuelta eliminando el ultimo digito. Se pone n >= 100, porque para todo n de dos digitos ya lo comprobamos arriba
                           | otherwise = True --si llegó hasta aca es porque n ya tiene 2 digitos y esos dos son iguales (si no hubiera muerto en la tercer guarda) -> True