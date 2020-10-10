import Data.Bool
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

--concatFilter :: [[a]] -> [a]
--concatFilter = filter 


sonCincos :: [Int] -> Bool
sonCincos []     = True
sonCincos (n:ns) = n == 5 && sonCincos ns

sonCincosFoldr :: [Int] -> Bool
sonCincosFoldr = foldr (\n -> \b -> n==5 && b) True 


concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = (++) xs (concat2 xss)

len' :: [a] -> Int
len' [] = 0
len' (x:xs) = 1 + len' xs

len :: [a] -> Int
len = foldr (\_ x -> x + 1) 0


cantTotal :: [[a]] -> Int
cantTotal []       = 0
cantTotal (xs:xss) = len xs + cantTotal xss

cantTotal' :: [[a]] -> Int
cantTotal' = foldr ((+) . len) 0

