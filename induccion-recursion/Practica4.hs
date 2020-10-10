import Practica1

--toma dos n ́umeros x, y y devuelve el primer divisor de y mayor que x.
nextDiv :: Int -> Int -> Int
nextDiv x y 
            | dividesTo (x+1) y = x+1 
            | otherwise = nextDiv (x+1) y  

--toma un numero y devuelve la suma de sus divisores.

--estrategia 1: recorrido desde i = 1 hasta m
---------------      sumo i si i es dividor de m
sumDivs :: Int -> Int
sumDivs m = sumAux 1 
                where sumAux n 
                          | n < m = divisorSi_DivideA_ n m + sumAux (n+1) 
                          |  otherwise = m
divisorSi_DivideA_  :: Int -> Int -> Int
divisorSi_DivideA_ a b | dividesTo a b = a | otherwise = 0

--estrategia 2: recorrido desde i = 1 hasta m por saltos (pasando por los que son divisores)
--------------       sumo i siempre
sumDivs' :: Int -> Int
sumDivs' m = sumAux 0 where sumAux n 
                               | n < m = n + sumAux (nextDiv n m) 
                               | otherwise = m

--decide si un número es primo.
prime n  = nextDiv 1 n == n
