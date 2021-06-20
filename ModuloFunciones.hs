module ModuloFunciones
where
    {-Aca van las funciones que me pintan, gil-}
    fact :: Int -> Int
    fact n | n == 0 = 1
           | n >  0 = n * fact(n-1)
           | otherwise = undefined

    --suma geometrica
    f2 :: Int -> Float -> Float
    f2 0 _ = 0
    f2 n q = q^n + (f2 (n-1) q)

    --Implementar una funcion que determine si todos los dıgitos de un numero son iguales.
    digitosIguales :: Int -> Bool
    digitosIguales n | n >= 0 && n < 10 = True
                     | (mod n 10) == (mod (div n 10) 10) = digitosIguales(div n 10)
                     | otherwise = False 
    fibo :: Int -> Int
    fibo n | n == 0 = 0
           | n == 1 = 1
           | otherwise = fibo(n-1) + fibo(n-2)