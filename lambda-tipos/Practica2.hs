--práctica 2: Expresiones y alores. Tipos. Notación Lambda.

data ColorPrimario = Rojo | Azul | Verde deriving (Show, Eq)

data ColorSecundario = Cian | Magenta | Amarillo   deriving (Show, Eq)

mezclar :: ColorPrimario -> ColorPrimario -> ColorSecundario

mezclar Rojo Azul = Magenta
mezclar Rojo Verde = Amarillo
mezclar Azul Rojo = Magenta
mezclar Azul Verde = Cian
mezclar Verde Rojo = Amarillo
mezclar Verde Azul = Cian
mezclar _ _ = error "esa combinación no genera un color secundario"

data Punto = Punto2D Int Int | Punto3D Int Int Int deriving (Show, Eq)

suma :: Punto -> Punto -> Punto
suma (Punto2D x1 y1) (Punto2D x2 y2) = Punto2D (x1+x2) (y1+y2)
suma (Punto3D x1 y1 z1) (Punto3D x2 y2 z2) = Punto3D (x1+x2) (y1+y2) (z1+z2)

modulo :: Punto -> Float 
modulo (Punto2D x y) = (sqrt.fromIntegral) (x^2 + y^2)
modulo (Punto3D x y z) = (sqrt.fromIntegral) (x^2 + y^2 + z^2)

coordenadaX :: Punto -> Int

coordenadaX (Punto2D x y) = x
coordenadaX (Punto3D x y z) = x

coordenadaY :: Punto -> Int

coordenadaY (Punto2D x y) = y
coordenadaY (Punto3D x y z) = y

distancia :: Punto -> Punto -> Float
distancia (Punto2D x1 y1) (Punto2D x2 y2) = modulo (Punto2D (x1-x2) (y1-y2))
distancia (Punto3D x1 y1 z1) (Punto3D x2 y2 z2) = modulo (Punto3D (x1-x2) (y1-y2) (z1-z2))


type Radio = Float 
data Figura = Circulo Punto Radio | Rectangulo Punto Punto deriving (Show, Eq)

-- pasar a notacion lambda

{-
smaller = \case (x,y,z) | x <= y && x <= z -> x
                 | y <= x && y <= z -> y
                 | z <= x && z <= y -> z
                 | otherwise -> x
-}
--second es una función que recibe un parámetro de tipo a,
-- y retorna una función anónima que recibe un parámetro de tipo  y retorna algo del mismo tipo
second x = \x -> x 
second1 = \x -> \y -> y 
second2 = \x y -> y


andThen True y = y
andThen False y = False

andThen' = \b y  -> if b then y else b


-- de notacion lambda a Haskell

iff = \x -> \y -> if x then not y else y
iff' x y = if x then not y else y

alpha = \x -> \x -> x
alpha' x y = y
