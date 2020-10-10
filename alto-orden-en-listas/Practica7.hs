import Data.Char(ord)

-- EJERCICIO 1
sum' = foldr (+) 0

any = foldr (||) False

codes = foldr ((:).(ord)) []

codes' = map ord

reminders n = map (mod n)

--squares :: [a] -> [a]
squares = map (\x -> x*x)

lengths = map (length)

order = filter (\(x,y) -> x < 3*y)

morethan n = filter ((>n).length)

pairs = filter (\x -> x `mod` 2 == 0)

compare1 [] [] = True
compare1 [] ys = False
compare1 xs [] = False
compare1 (x:xs) (y:ys) = x == y  && compare1 xs ys

--EJERICIO 2

recr z f [] = z 
recr z f (x:xs) = f x xs (recr z f xs)

--pal:: [a] -> Bool
pal xs = reverse xs == xs

--hs, que cuenta la cantidad de palabras que empiezan con h en una lista dada. 
hs:: [Char] -> Int
hs = length . damelash

damelash = filter (\x -> x == 'h')
			
--avgLength, que calcula la longitud promedio de las palabras de una lista.
avgLength:: [String] -> Int
avgLength xs = (sum . lengths) xs `div` length xs    			

--adjacents, que tome una lista y retorna la lista de todos los pares ordenados de elementos adyacentes, 
--por ejemplo: adjacents [2, 1, 11, 4] = [(2,1),(1,11),(11,4)]
adjacents:: [a] -> [(a,a)]
adjacents = recr [] (\x xs r -> if null xs then [] else (x,head xs): r)
--diffAdj, que toma una lista de numeros y devuelve la lista de los pares ordenados de todos los numeros
-- adyacentes cuya diferencia es par.
diffAdj:: [Int] -> [(Int,Int)]
diffAdj = filter (\(x,y) -> x - y `mod` 2 == 0) . adjacents
diffAdj' xs = filter (\(x,y) -> x - y `mod` 2 == 0)  (adjacents xs)

--remDups, que devuelve una lista con los mismos elementos que la original, pero 
--eliminando todos aquellos valores que fueran adyacentes e iguales, dejando una sola ocurrencia de cada uno. 
-- [2, 1,11, 4,4] = [(2,1),(1,11),(11,4), (4,4)] = 2,1,11,4
remDups:: Eq a => [a] -> [a]
remDups = foldr (\x xs -> if not (null xs) && x == head xs then xs else x:xs) []
remDups':: Eq a => [a] -> [a]
remDups' = recr [] (\x xs r -> if not (null xs) && x == head xs then r else x:r)

--primes, que dado un entero n devuelve una lista con los n primeros primos.

primes' n = take n (filter esPrimo [2..])

esPrimo n = True --hay que importar la función
--esPrimo = \n -> nextDiv 1 n == n 

--EJERCICIO 4

filter' :: (a->Bool)->[a]->[a]
filter' f  = concat.(map (\x -> if f x then [x] else [] ))


--EJERICIO 5
--5. Deﬁnir las funciones takewhile, que devuelve el segmento inicial mas largo de una lista de elementos que 
--veriﬁcan una condicion dada, y dropwhile, que devuelve el segmento de la lista que comienza con el primer
--elemento que no veriﬁca la condicion dada

--takewhile = 
