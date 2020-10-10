module Principales 
(sum',lenl,recr, isEven) 
where

import Data.Char(ord)

sum' :: [Int] -> Int
sum' = foldr (+) 0

--lenl: describe la longitud de una lista
lenl :: [a] -> Int
lenl = foldr (\_ x -> x+1) 0

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

-- isEven: describe verdadero si el número es par
isEven :: Int -> Bool
isEven = \ n -> mod n 2 == 0

--any: devuelve True si algún elemento de una lista es True
any :: [Bool] -> Bool
any = foldr (||) False

--all: devuelve True si todos los elementos de una lista sin True
all :: [Bool] -> Bool
all = foldr (&&) True

--codes: dada una lista de caracteres, devuelve la lista de sus códigos
codesMap :: [Char] -> [Int]
codesMap = map ord

codesFoldr :: [Char] -> [Int]
codesFoldr = foldr ((:).ord) []

--remainders: dada una lista de númros, devuelve los restos de la división por un número

remainders n [] = []
remainders n (x:xs) = mod x n : remainders n xs

remaindersMap :: Int -> [Int] -> [Int]
remaindersMap n = map (`mod` n)

remainders' :: Int -> [Int] -> [Int]
remainders' n = foldr ((:). resto n) []
resto n x = mod x n
resto n x = mod x n
resto n x = mod x n
resto n x = mod x n
resto n x = mod x n
resto n x = mod x n
resto n x = mod x n
resto n x = mod x n
--squares: dada una lista de números, devuele la lista de sus cuadrados
squares :: [Float] -> [Float]
squares = map (^2)


--lengths: dada una lista de listas, devuelve la lista de sus longitudes
lengths :: [[a]] -> [Int]
lengths = foldr ((:).lenl) []

--order: dada una lista de pares ordenados de números, devuelve la lsta de aquellos cuya primer componente es menor que el triple de la segunda
order :: [(Integer, Integer)]-> [(Integer,Integer)]
order = filter (\(x,y) -> x<=3*y)

--pairs: dada una lista de números, devuelve  a lista de aquellos que son pares
pairs :: [Integer]->[Integer]
pairs = filter (\x -> mod x 2 == 0)
pairs2 = filter ((==0).(`mod` 2)) --no m agrada :X

--chars:dada una lista de caracteres, devuelve la lista de aquellos que son letras
--chars :: [Char] -> [Char]
--chars = filter isAlpha

--moreThan: dada una lista de xss y un número n, devuelve la lista de aquellas listas de xss que tienen logitud mayor que n

moreThan :: [[a]] -> Int -> [[a]]
moreThan xss n = filter (\xs -> length xs > n) xss

moreThan' :: Int -> [[a]] -> [[a]]
moreThan' n= filter (\xs -> length xs > n)

