--Práctica 9.  Temas: Patrones genéricos de recursión. Funciones sobre árboles.

--Ejercicio 1
data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving Show

foldTip :: (a->b)->(b -> b -> b) -> TipTree a -> b

foldTip f g (Tip x) = f x
foldTip f g (Join t1 t2) = g (foldTip f g t1) (foldTip f g t2)

leaves = foldTip (const 1) (+)

height = foldTip (const 0) (\h1 h2 -> 1 + max h1 h2)

nodes = foldTip (const 0) (\n1 n2 -> 1 + n1 + n2)

mirrorTip :: TipTree a -> TipTree a
mirrorTip = foldTip Tip (flip Join)

walkover = foldTip (\h -> [h]) (++)

mapTip f = foldTip (Tip . f) Join

tipTreeExample = Join (Tip 1) (Join (Tip 2) (Tip 3))
pot = \x->x*x


--Ejercicio 2
data BinTree a = Empty | Bin a (BinTree a) (BinTree a) deriving Show

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f z Empty = z
foldBin f z (Bin x t1 t2) = f x (foldBin f z t1) (foldBin f z t2)

nodesBin :: BinTree a -> Int
nodesBin = foldBin (\x n1 n2 -> 1 + n1 + n2) 0

heightBin :: BinTree a -> Int
heightBin = foldBin (\x h1 h2 -> 1 + max h1 h2) 0

mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f = foldBin (Bin . f) Empty

mirrorBin :: BinTree a -> BinTree a
mirrorBin = foldBin (flip . Bin) Empty

binTreeExample = Bin 1 Empty (Bin 2 Empty (Bin 3 Empty Empty))

--Ejercico 3

data GenTree a = Gen a  [GenTree a] deriving Show

foldGen0 :: (a -> [b] -> b) -> GenTree a -> b

foldGen0 f (Gen x ts) = f x (map (foldGen0 f) ts)

foldGen1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GenTree a -> b

foldGen1 g f z (Gen x ts) = g x (foldr f z (map (foldGen1 g f z) ts))

foldGen2 :: (a -> c -> b) -> ([b] -> c) -> GenTree a -> b
foldGen2 g k (Gen x ts) = g x (k (map (foldGen2 g k) ts))

--Ejercicio 4
mapGen :: (a -> b) -> GenTree a -> GenTree b
mapGen f = foldGen1 (Gen . f) (:) []

heightGen :: GenTree a -> Int
heightGen = foldGen2 (\_ h -> 1 + h) (maxWith 0)
maxWith x [] = x
maxWith x xs = maximum (x:xs)

mirrorGen :: GenTree a -> GenTree a
mirrorGen = foldGen1 Gen (\x h -> h ++ [x]) []
mirrorGen' = foldGen2 Gen reverse

genTreeExample = Gen 1 [Gen 2 [Gen 4 []], Gen 3 [Gen 5 [], Gen 6 [], Gen 7 []]]

--Ejercicio 5
data GenExp a = Leaf a | Un (GenExp a) | BinG (GenExp a) (GenExp a)
foldGenExp l u b (Leaf x) = l x
foldGenExp l u b (Un t) = u (foldGenExp l u b t)
foldGenExp l u b (BinG t1 t2) = b (foldGenExp l u b t1) (foldGenExp l u b t2)

data NExp = Num Int | Sum NExp NExp | Sub NExp NExp | Neg NExp
foldNExp num sum sub neg (Num n) = num n
foldNExp num sum sub neg (Sum e1 e2) = sum (foldNExp num sum sub neg e1) (foldNExp num sum sub neg e2)
foldNExp num sum sub neg (Sub e1 e2) = sub (foldNExp num sum sub neg e1) (foldNExp num sum sub neg e2)
foldNExp num sum sub neg (Neg e) = neg (foldNExp num sum sub neg e)
evalNExp = foldNExp id (+) (*) (*(-1)) 

--definimos otro recorrido para NExp usando la estructura siguiente
data AB a b = Leaf1 b | Branch a (AB a b) (AB a b)
foldAB :: (b -> c) -> (a -> c -> c -> c) -> AB a b -> c
foldAB f g (Leaf1 y) = f y
foldAB f g (Branch x t1 t2) = g x (foldAB f g t1) (foldAB f g t2)

type NExpAB = AB Op Int
data Op = Numero | Suma | Producto | Negativo
--evalNExpAB = foldAB opUnaria opBinaria
opUnaria Numero = id
opUnario Negativo = (*(-1)) 
opBinaria Suma = (+)
opBinaria Producto = (*)


data Either' a b = Left' a | Right' b
foldEither' left right (Left' x) = left x
foldEither' left right (Right' x) = right x

data Nat = Zero | Succ Nat
foldNat f z Zero = z
foldNat f z (Succ n) = f (foldNat f z n)
