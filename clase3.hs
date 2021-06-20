--Recursión

--por hardware
factorial :: Int -> Int
factorial n | n == 0 = 1
            | n >  0 = n * factorial(n-1)
            | n <  0 = undefined

--por software (tarda mas, pero mayor cant de valores)
superFactorial :: Integer -> Integer
superFactorial n | n == 0 = 1
                 | n >  0 = n * superFactorial(n-1)
                 | n <  0 = undefined

esPar :: Int -> Bool
esPar n | n == 0 = True
        | n == 1 = False
        | otherwise = esPar(n-2)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

parteEntera :: Float -> Integer
parteEntera n | n < 0 = undefined
              | n < 1 = 0
              | otherwise = 1 + parteEntera(n-1)

{-Escribir una función para determinar si un número natural es m´ultiplo de 3. 
No está permitido utilizar mod ni div-}
mult3 :: Int -> Bool
mult3 n | n < 0 || n == 1 = False
        | n == 0 = True
        | otherwise = mult3(n-3)

{-Implementar la funcion sumaImpares :: Int -> Int que dado n ∈ N sume los 
primeros n numeros impares-}
sumaImpares :: Int -> Int
sumaImpares n | n < 0 = undefined
              | n == 0 = 0
              | otherwise = impar + sumaImpares(n-1)
            where impar = n*2 -1

{-Escribir una funcion medioFact que dado n ∈ N calcula 
n!! = n (n − 2)(n − 4) · · · -}
medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n < 0 = undefined
            | otherwise = n * medioFact(n-2)
-- Escribir una funcion que determine la suma de dıgitos de un numero positivo
sumaDigito :: Int -> Int
sumaDigito n | n >= 0 && n < 10 = n
             | otherwise = digito + (sumaDigito(mod n 10))
             where digito = div n 10
             
--Implementar una funcion que determine si todos los dıgitos de un numero son iguales.
digitosIguales :: Int -> Bool
digitosIguales n | n >= 0 && n < 10 = True
                 | (mod n 10) == (mod (div n 10) 10) = digitosIguales(div n 10)
                 | otherwise = False