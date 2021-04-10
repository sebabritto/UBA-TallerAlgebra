-- Variables tipo

--Funciones con variables tipo
identidad :: t -> t
identidad x = x 

primero :: tx -> ty -> tx
primero x y = x

segundo :: tx -> ty -> ty
segundo x y = y

constante5 :: tx -> ty -> tz -> Int
constante5 x y z = 5

mismoTipo :: t -> t -> Bool
mismoTipo x y = True

--triple x = 3*x -- Tipo numero

{-maximo x y | x >= y = x  -- Tipo orden
           | otherwise = y-}

--distintos x y = x /= y -- Tipo igualdad

--Clases de tipo

triple :: (Num t) => t -> t 
triple x = 3*x

maximo :: (Ord t) => t -> t -> t
maximo x y | x >= y = x
           | otherwise = y

distintos :: (Eq t) => t -> t -> Bool
distintos x y = x /= y

--Cant de raices de x^2 + bx + c
cantDeSol :: (Num t, Ord t) => t -> t -> Int
cantDeSol b c | d > 0 = 2
              | d == 0 = 1 
              | otherwise = 0
              where d = b^2 - 4*c

--Ejercitacion conjunta
{-f1 x y z = x**y + z <= x + y**z

f2 x y = (sqrt x) / (sqrt y)

f3 x y = div (sqrt x) (sqrt y)

f4 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z

f5 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y

cinco :: Int
cinco = 5
-}
--Tuplas
{-
suma :: (Float , Float) -> (Float , Float) -> (Float , Float)
suma v w = (( fst v) + (fst w), (snd v) + (snd w))
-}
--Pattern matching
--suma (vx , vy) (wx , wy) = (vx + wx , vy + wy)

--Pattern matching sobre tuplas
esOrigen:: (Float ,Float) -> Bool
esOrigen (0, 0) = True
esOrigen (_, _) = False

angulo0:: (Float , Float) -> Bool
angulo0 (_, 0) = True
angulo0 (_, _) = False

{-No podemos usar dos veces la misma variable
angulo45 :: (Float , Float) -> Bool
angulo45 (x,x) = True
angulo45 (_,_) = False
-}
angulo45 :: (Float , Float) -> Bool
angulo45 (x,y) = x == y

patternMatching :: (Float , (Bool , Int), (Bool , (Int , Float))) -> (Float , (Int , Float))
patternMatching (f1 , (True , _), (_, (0, f2))) = (f1 , (1, f2))
patternMatching (_ , _          , (_, (_, f ))) = (f, (0, f))

--Parametros vs Tuplas
suma :: (Float , Float) -> (Float , Float) -> (Float , Float)
suma (vx , vy) (wx , wy) = (vx+wx , vy + wy)

-- | normaVectorial2 x y es la norma de (x,y)
normaVectorial2 :: Float -> Float -> Float
normaVectorial2 x y = sqrt (x**2 + y**2)

-- | normaVectorial1 (x,y) es la norma de (x,y)
normaVectorial1 :: (Float , Float) -> Float
normaVectorial1 (x,y) = sqrt (x**2 + y**2)

norma1Suma :: (Float , Float) -> (Float , Float) -> Float
norma1Suma v1 v2 = normaVectorial1 (suma v1 v2)

norma2Suma :: (Float , Float) -> (Float , Float) -> Float
norma2Suma v1 v2 = normaVectorial2 (fst s) (snd s)
        where s = suma v1 v2 

