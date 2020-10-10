import Principales

--que devuelve una lista con los mismos elementos que la original, 
--pero eliminando todos aquellos valores que fueran adyacentes e iguales, 
--dejando una sola ocurrencia de cada uno.

remDupsRecr :: Eq a => [a] -> [a]
remDupsRecr = recr [] (\x xs r -> if not (null xs) && x == head xs then r else x:r)

remDupsFoldr :: Eq a => [a] -> [a]
remDupsFoldr = foldr (\x xs -> if not (null xs) && x == head xs then xs else x:xs ) []
