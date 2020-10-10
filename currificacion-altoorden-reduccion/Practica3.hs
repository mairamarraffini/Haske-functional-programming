-- CURRIFICACIÓN
---- correspondenia entre función de múltiples parámetros y una de alto orden que retorna una función intermedia que completa la tarea

--ALTO ORDEN:
----  es una función que al menos:
------ 1. toma uno o más funciones como argumentos
------ 2. retorna una función como resultado

-- REDUCCIÓN
----  mecanism para calcula el valor de una expresión. 
---- cómo se calcula?
------ 1. localiza un redex
------ 2. reemplaza
------ 3. se repite hasta que no haya redex (forma normal)
------ 
------ REDEX (Reducible expression): subexpresión que coincide con una instancia del lado izquierdo de una ecuación

-- ORDENES DE EVALUACIÓN
---- es el algoritmo de evaluación del redex a reducir
---- Existe más de una forma de reducir:
------- ORDEN APLICATIVO: primero los redex internos
------- ORDEN NORMAL: primero los redex externos

and' :: Bool -> Bool -> Bool
and' b1 b2 = b1 && b2

h :: (a->b)->(a->c)->a->(b,c)
h f g x = (f x, g x)

suma :: Int -> Int -> Int
suma n m = n + m

cinco :: (Int -> Int) -> Int  -> Int
cinco auxCinco x = auxCinco x +  2
auxCinco y = y + y

seis :: (a->b->c)->b->a->c
seis swap x y = swap y x

-- OPERADORES: son funciones
-- Los operadores como la suma "+", la resta "-", la muliplicación "*" son funciones.
--  Sus nombres no son una palabra, si no un sìmbolo
--  Las funciones comunes se aplican de forma  PREFIJA, es decir, delante de sus argumentos: "siguiente numero"
--  Los operadores se utilizan de forma INFIJA, es decir entre medio de sus argumentos: "3 + numero"
-- Haskel nos permite usar a los operaores de forma prefija, poniendo en evidencia que son funciones: "siguiente numero = (+) 1 numero" o "siguiente = (+) 1"
--
-- escribo al operador como una función
siguiente x = (+) 1 x
--mas bello, sin parámetro:
siguiente' = (+) 1 

-- 5.de notación Haskell a lambda
twice f x = f (f x)
twice' = \f -> \x ->f (f x)

flip :: (a->b->c)->b->a->c
flip f x y = f y x

flip' :: (a->b->c)->b->a->c
flip' = \f -> \x -> \y -> f y x

flip'' :: (a->b->c)->b->a->c
flip'' = \f x y -> f y x

inc :: Integer -> Integer
inc = (+) 1
inc' = \n -> (+) 1 n


-- dar el tipo de las siguientes funciones
fix f x = f (fix f) x

apply f x = f x 

fork (f,g) x = (f x, g x)
