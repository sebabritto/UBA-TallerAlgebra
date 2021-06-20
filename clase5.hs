import ModuloFunciones

prod :: Int -> Int -> Int
{-| Productoria que va desde d hasta h-}
prod d h | d == h = d
         | otherwise = h * prod d (h-1)

prod' :: Int -> Int -> Int
prod' d h | d == h = d
          | otherwise = d * prod' (d+1) h

sumaDivisores' :: Int -> Int
sumaDivisores' n = sumaDivisoresHasta' n 1

sumaDivisoresHasta' :: Int -> Int -> Int
sumaDivisoresHasta' n k | k == n = n
                        | n `mod` k == 0 = k + sumaDivisoresHasta' n (k+1)
                        | otherwise = sumaDivisoresHasta' n (k+1)

{-| Calcula el menor divisor (mayor de 1) de un natural-}
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo' :: Int -> Bool
esPrimo' n = n > 1 && not(tieneDivisoresDesde n 2)

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde n k | k == n = False
                        | otherwise = (n `mod` k == 0) || tieneDivisoresDesde n (k+1)
{-| Implementar menorFactDesde :: Int -> Int que dado m ≥ 1 encuentra el mínimo n ≥ m 
tal que n = k! para algún k-}
menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m | fact(i) >= m = fact(i)
                        | otherwise = menorFactDesdeDesde (i+1) m

{-|Implementar mayorFactHasta :: Int -> Int que dado m ≥ 1 encuentra el m´aximo
n ≤ m tal que n = k! para algun k.-}
mayorFactHasta :: Int -> Int
mayorFactHasta n = mayorFactHastaDesde n n

mayorFactHastaDesde :: Int -> Int -> Int
mayorFactHastaDesde n k | fact(k) <= n = fact(k)
                        | otherwise = mayorFactHastaDesde n (k-1)

{-|Implementar esFact :: Int -> Bool que dado n ≥ 0 decide si existe un numero entero
k ≥ 0 tal que n = k!-}

esFact :: Int -> Bool
esFact n = n >= 0 && existeFact n 0

existeFact :: Int -> Int -> Bool
existeFact n k | k > n = False
               | fact(k) == n = True
               | otherwise = existeFact n (k+1)

{-|Implementar esFibonacci :: Int -> Bool que dado un numero entero n ≥ 0 decide si n
es un numero de Fibonacci -}

esFibonacci :: Int -> Bool
esFibonacci n = n >= 0 && compFibo n 0

compFibo :: Int -> Int -> Bool
compFibo n k | fibo(k) > n = False
             | n == fibo(k) = True
             | otherwise = compFibo n (k+1)

{-|Implementar esSumaInicialDePrimos :: Int -> Bool que dado un numero entero n ≥ 0
decide si n es igual a la suma de los m primeros numeros primos, para algun m.-}

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = n >= 0 && sumaPrimos (raiz n) == n

raiz :: Int -> Int
raiz n = aux n
  where
    aux x
      | x*x > n = aux (x - 1)
      | otherwise = x

sumaPrimos :: Int -> Int
sumaPrimos n | n == 0 = 0 
             | otherwise = nEsimoPrimo(n) + sumaPrimos(n-1)

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde(n+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde(1 + nEsimoPrimo(n-1) )

{-|Comprueba si el valor ingresado es primo-}
esPrimo :: Int -> Bool
esPrimo n | n == 1 = False
          | otherwise = n > 1 && not(tieneDivisores n 2)

{-|Comprueba si el numero ingresado tiene mas de 1 divisor-}
tieneDivisores :: Int -> Int -> Bool
tieneDivisores n m | n == m = False
                   | otherwise = (n `mod` m == 0) || tieneDivisores n (m+1)

{-|Calcula la suma de los divisores un entero positivo-}
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

{-|Devuelve la suma de los divisores de un n´umero hasta cierto punto.-}
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 | n2 >= n1 && n1 >= 1 = valorMax n1 n2 n2
                   | otherwise = 0

valorMax :: Int -> Int -> Int -> Int
valorMax n1 n2 n3 | (n3 < n1) && (sumaDivisores(n3) > n2) = 0
                  | (sumaDivisores(n3) >= n1) && (sumaDivisores(n3) <= n2) = n3
                  | otherwise = valorMax n1 n2 (n3-1)

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 | n2 >= n1 && n1 >= 1 = valorMin n1 n2 n1
                   | otherwise = 0

valorMin :: Int -> Int -> Int -> Int
valorMin n1 n2 n3 | (n3 < n1) && (sumaDivisores(n3) > n2) = 0
                  | (sumaDivisores(n3) >= n1) && (sumaDivisores(n3) <= n2) = n3
                  | otherwise = valorMax n1 n2 (n3+1)

{-|Los numeros naturales a y b forman un par de primos gemelos si b = a + 2 y tanto a 
como b son primos. Implementar primosGem :: Int -> Int que dado n, devuelve la cantidad 
de pares de primos gemelos (a, b) que verifican b ≤ n. Por ejemplo: primosGem 5 = 1 
(porque 3 y 5 es un par de primos gemelos) primosGem 14 = 3 (porque 3 y 5, 5 y 7, y 
11 y 13 son tres pares de primos gemelos)-}
primosGem :: Int -> Int
primosGem p | p == 1 = 0
                | otherwise = cantPrimosGem(p) + primosGem(p-1)

cantPrimosGem :: Int -> Int
cantPrimosGem c | c <= 2 = 0
                | esPrimo(c) && esPrimo(c-2) = 1
                | otherwise = 0
{-|Conjetura de los primos gemelos: Existen infinitos pares de primos gemelos. 
Implementar la funcion proxPrimosGem :: Int -> (Int,Int) que dado n devuelve el primer 
par de gemelos (a, b) tal que a > n
-}
proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem n = primosGeme n (n+1)

primosGeme :: Int -> Int -> (Int, Int)
primosGeme n m | m > n && esPrimo(m) && esPrimo(m-2) = (m, m-2)
               | otherwise = primosGeme n (m+1)

{-|Conjetura Lothar Collatz-}

largoSecuencia :: Int -> Int
largoSecuencia n | n > 1 = cantSec n
                 | n == 1 = 3
                 | otherwise = 0

cantSec :: Int -> Int
cantSec a | a == 1 = 0
          | a `mod` 2 == 0 = 1 + cantSec(a `div` 2)
          | otherwise = 1 + cantSec(3*a + 1)