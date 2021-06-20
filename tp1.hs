
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | n `mod` 2 == 0 && n > 2 = n == (fst (descomposicionEnPrimos n)) + (snd(descomposicionEnPrimos n))
                    | otherwise = False

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n | (n `mod` 2) /= 0 && n <= 2 = False
                          | n == 4 = True
                 --   MAL      | satisfaceGoldbach n  = satisfaceGoldbach (n-2)
                          | otherwise = satisfaceGoldbach n && verificarConjeturaHasta (n-2)

descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n | (n `mod` 2) == 0 && (n > 2) = sumaDePrimos n (n-2) 2
                         | otherwise = (0,0)

numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n | n `mod` 2 == 0 && n > 2 = contadorParPrimos n (n-2) 2
                           | otherwise = 0

{-|Devuelve la cantidad de pares de numeros primos tales que a + b == n-}
contadorParPrimos :: Integer -> Integer -> Integer -> Integer
contadorParPrimos n a b | a == 1 || b == n = 0
                        | esPrimo(a) && esPrimo(b) && a + b == n = 1 + contadorParPrimos n (a-1) (b+1)
                        | otherwise = contadorParPrimos n (a-1) (b+1)

{-|Devuelve una tupla de dos numeros primos que al sumarlos me dan el valor de n-}
sumaDePrimos :: Integer -> Integer -> Integer -> (Integer, Integer)
sumaDePrimos n a b | a < n && b < n && esPrimo(a) && esPrimo(b) && (n == (a + b)) = (a, b)
                   | otherwise = sumaDePrimos n (a-1) (b+1)

{-|Comprueba si el valor ingresado es primo-}
esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | otherwise = n > 1 && not(tieneDivisores n 2)

{-|Comprueba si el numero ingresado tiene mas de 1 divisor-}
tieneDivisores :: Integer -> Integer -> Bool
tieneDivisores n m | n == m = False
                   | otherwise = (n `mod` m == 0) || tieneDivisores n (m+1)