--Practica 1: Introducción a la sintaxis de Haskell y ambiente de Hugs
module Practica1 
(dividesTo,isMultiple)
where

dividesTo :: Int -> Int -> Bool
dividesTo = \x y -> mod y x == 0

isMultiple :: Int -> Int -> Bool
isMultiple = \x y ->  dividesTo y x
isMultiple' = \x y -> mod x y == 0

power4 :: Int -> Int
power4 = \x -> x * x * x * x

power4' :: Int -> Int
power4' x = sqr x * sqr x where sqr y = y * y


f :: (Int -> Int) -> Int
f g = g 2 + 2 where g x = x * 3 

seven x = 7

compose f g x = f x (g x)

pairFunc (f1,f2) x y = (f1 (f2 x), f2 (f1 y))

a = if False then True else False

b x  = (1 < x) && (x==2)
