--hs: cuenta la cantidad de palaras que empiezan con h en una lista  dada
hs1 :: [String] -> Int
hs1 [] = 0
hs1 (s:ss) = (\str -> if (take 1 str)=="h" then 1 else 0) s + hs1 ss

hs2 :: [String] -> Int
hs2 [] = 0
hs2 (s:ss) =  unoSiEmpiezaConH s + hs2 ss

hs3 :: [String] -> Int
hs3 [] = 0
hs3 (s:ss) = ((+).unoSiEmpiezaConH) s (hs2 ss)

hsFoldr :: [String] -> Int
hsFoldr = foldr ((+).unoSiEmpiezaConH) 0
--hs :: [String] -> Int
--hs = foldr ((+).unoSiEmpiezaConH) 0

unoSiEmpiezaConH str | (take 1 str) == "h"   = 1
                     | otherwise             = 0


lenl :: [a] -> Int
lenl = foldr (\_ n -> n +1) 0
