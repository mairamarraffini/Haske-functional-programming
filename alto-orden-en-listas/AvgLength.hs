-- avgLength: calcula la longitud promedio de las palabras de una lista


lengthList :: [a] -> Int
lengthList = foldr (\_ x -> x+1) 0
sumList = foldr (+) 0

strLength :: [String] -> Int
strLength = foldr ((+).length) 0

avgLength :: [String] -> Int
avgLength xs = strLength xs `div` lengthList xs


