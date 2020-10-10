--practica 5
--tipos algebraicos
--pattern matching
--LISTAS:
---- se define inductivamente al conjunto [a] con las siguientes reglas:
---- 1. [] :: [a]
---- 2. si x :: a y xs :: [a] entonces x:xs :: [a]

import Data.Char(ord)
import Data.Maybe

--sum: suma los elementos de una lista de números. 
sum' []     = 0
sum' (x:xs) = x + sum xs

--any: devuelve True si algún elemento de una lista es True.
any' []     = False
any' (x:xs) = x || any' xs 

--allTrue: describe verdadero si todos los elementos de la lista son True.
allTrue []     = True
allTrue (x:xs) = x && allTrue xs

--allFalse: describe verdadero si todos los elementos de la lista describen Falso
allFalse []     = True
allFalse (x:xs) = not x && allFalse xs

--anyFalse : describe Verdadero si algun elemento de la lista es False
anyFalse = not.allTrue

--anyTrue: describe verdadero si algún elemento de la lista es True
anyTrue = not.allFalse

--codes: dada una lista de caracteres, devuelve la lista de sus códigos.
codes :: [Char] -> [Int]
codes []     = []
codes (c:cs) = ord c : codes cs

--remainders:  dada una lista de números, devuelve los restos de su división por un número
remainders :: Int -> [Int] -> [Int]
remainders d []     = []
remainders d (n:ns) = mod n d : remainders d ns

--squares: dada una lista de números, devuelve la lista de sus cuadrados.
squares :: [Int] -> [Int]
squares []      = []
squares (n:ns)  = n*n : squares ns

--lengths: dada una lista de listas, devuelve la lista de sus longitudes.
lengths :: [[a]] -> [Int]
lengths []       = []
lengths (xs:xss) = lenl xs : lengths xss

lenl [] = 0
lenl (x:xs) = 1 + lenl xs


--order: dada una lista de pares ordenados de números, devuelve la lista de aquellos
--cuya primer componente es menor que el triple de la segunda.
order :: [(Int,Int)] ->[(Int,Int)]
order []     = []
order (n:ns) = if esMenorQueElTriple n 
               then n:order ns
               else order ns

esMenorQueElTriple :: (Int,Int)->Bool
esMenorQueElTriple n = fst n < 3 * snd n

--pairs: dada una lista de números, devuelve la lista con aquellos que son pares.
pairs :: [Int] -> [Int]
pairs []     = []
pairs (n:ns) = if esPar n 
               then n:pairs ns 
               else pairs ns

esPar :: Int -> Bool
esPar  = \n -> mod n 2 == 0 
 
--moreThan: dada una lista de listas xss y un número n, devuelve la lista de aquellas listas de xss que tienen longitud mayor que n.
moreThan :: Int -> [[a]] -> [[a]]
moreThan n []       = []
moreThan n (xs:xss) = if esLongListaMayorA n xs 
                      then xs:moreThan n xss 
                      else moreThan n xss

esLongListaMayorA :: Int -> [a] -> Bool 
esLongListaMayorA n xs = lenl xs > n
