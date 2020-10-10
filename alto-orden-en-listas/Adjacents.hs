import Principales

--que tome una lista y retorna la lista de todos los pares ordenados de
--elementos adyacentes, por ejemplo:
--adjacents [2, 1, 11, 4] = [(2,1),(1,11),(11,4)]-

adjacents :: [a] -> [(a,a)]
adjacents = recr [] (\x xs r -> if null xs then [] else (x,head xs):r)


--que toma una lista de números y devuelve la lista de los pares ordenados
--de todos los números adyacentes cuya diferencia es par.

diffAdj :: [Int] -> [(Int, Int)]
diffAdj = (filter (\(x,y) -> isEven (x-y))) . adjacents


