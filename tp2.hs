type Posicion = [Int]

type Jugada = (Int, Int)

{-|Ejercicio 1: recibe un posicion p, una jugada valida j y devuelve la posicion
obtenida al realizar ducha jugada-}
jugar :: Posicion -> Jugada -> Posicion
jugar p j | fst j == 0 = p
          | fst j == 1 && head p == snd j = tail p 
          | fst j == 1                    = (head p - snd j) : tail p
          | otherwise                     = head p : jugar (tail p) (fst j - 1, snd j)

{-|Ejercicio 2: recibe una posicion p y devuelve el conjunto de jugadas validas a partir de p-}
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas p  = posiblesJugadasLista p (1,1)

posiblesJugadasLista :: Posicion -> Jugada -> [Jugada]
posiblesJugadasLista [] _  = []
posiblesJugadasLista p (x,y) | y < head p = (x,y) : posiblesJugadasLista p (x,y+1)
                             | otherwise  = (x,y) : posiblesJugadasLista (jugar p (1,y)) (x+1,1)

{-|Ejercicio 3: decide si una posicion p es ganadora-}
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = esGanadora p (posiblesJugadas p)

esGanadora :: Posicion -> [Jugada] -> Bool
esGanadora p j | p == [] || j == [] = False
               | otherwise = not(esGanadora (jugar p (head j)) (posiblesJugadas (jugar p (head j)))) || (esGanadora p (tail j)) 

{-|Ejercicio 4: recibe una posicion ganadora p y devuelve una jugada que dejaria
al rival en una posicion no ganadora-}
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p | esPosicionGanadora p == False = (0,0)
                 | otherwise = jugadaGanadoraAux p (posiblesJugadas p)


jugadaGanadoraAux :: Posicion -> [Jugada] -> Jugada
jugadaGanadoraAux p j | (esPosicionGanadora (jugar p (head j))) == False = head j
                      | otherwise = jugadaGanadoraAux p (tail j)

{-|Ejercicio 5: recibe una posicion p (no necesariamente ganadora) y devuelve la
cantidad de jugadas ganadoras partiendo de p-}
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0
numeroDeJugadasGanadoras p = numeroDeJugadasGanadorasAux p (posiblesJugadas p)

numeroDeJugadasGanadorasAux :: Posicion -> [Jugada] -> Int
numeroDeJugadasGanadorasAux p j | j == [] = 0
                                | (esPosicionGanadora (jugar p (head j))) == False = 1 + numeroDeJugadasGanadorasAux p (tail j)
                                | otherwise = numeroDeJugadasGanadorasAux p (tail j)