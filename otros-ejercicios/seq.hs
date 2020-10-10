import Data.Maybe (isNothing)

-- #4.

data Seq e = Nil | Unit e | Cat (Seq e) (Seq e) deriving Show

seqExample = Cat (Cat Nil (Unit 1)) (Cat (Unit 2) Nil)

-- appSeq: toma dos secuencias y devuelve su concatenación

appTwoSeq :: Seq e -> Seq e -> [e]
appTwoSeq (Seq e) (Seq e) = appSeq (Seq e) ++ appSeq (Seq e)

appSeq :: Seq e -> [e]
appSeq Nil         = []
appSeq (Unit x)    = [x]
appSeq (Cat s1 s2) = appSeq s1 ++ appSeq s2

--lenSeq: calcula la cantidad de elementos de una secuencia
lenSeq :: Seq e -> Int
lenSeq Nil          = 0
lenSeq (Unit x)     = 1
lenSeq (Cat s1 s2)  = lenSeq s1 + lenSeq s2

--revSeq: toma una secuencia e invierte sus elementos
revSeq :: Seq e -> Seq e
revSeq Nil          = Nil
revSeq (Unit x)     = Unit x
revSeq (Cat s1 s2)  = Cat (revSeq s2) (revSeq s1)

--headSeq: toma una secuencia y devuelve su primer elemento (el de más a la izq)
headSeq :: Seq e -> Maybe e
headSeq Nil          = Nothing
headSeq (Unit x)     = Just x
headSeq (Cat Nil s2) = headSeq s2
headSeq (Cat s1 s2)  = if hayElem then elem else headSeq s2
    where elem = headSeq s1
          hayElem = not $ isNothing elem


--conSeq: toma un elemento y una secuencia y devuelve la secuencia que tiene al elemento dado como cabeza y a la secuencia dada como cola.

--normSeq: elimina todos los Nil innecesarios de una secuencia.
--normSeq :: Seq e -> Seq e
normSeq Nil           = Nil
normSeq (Unit x)      = Unit x
normSeq (Cat Nil Nil) = Nil
normSeq (Cat s1 Nil)  = normSeq s1
normSeq (Cat Nil s2)  = normSeq s2
normSeq (Cat s1 s2)   = Cat (normSeq s1) (normSeq s2)

normSeq4 :: Seq a -> Seq a
-- por recursión sobre la estructura de la lista
normSeq4 Nil = Nil
normSeq4 (Unit x) = Unit x
normSeq4 (Cat s1 s2) = catNorm (normSeq4 s1) (normSeq4 s2)
    where catNorm Nil Nil = Nil
          catNorm Nil s = s
          catNorm s Nil = s
          catNorm s1 s2 = Cat s1 s2

normSeqExample = Cat (Cat Nil (Unit 1)) Nil

--eqSeq: toma dos secuencias y devuelve True sii ambas contienen los mismos valores, en el mismo orden y en la misma cantidad
--eqSeq :: Seq e -> Seq e -> Bool
eqSeq Nil Nil                 = True
eqSeq (Unit x) (Unit y)       = x==y
eqSeq (Cat s1 s2) (Cat s3 s4) = (eqSeq s1 s3) && (eqSeq s2 s4)
eqSeq _ _                     = False

eqSeqExample1 = Cat (Cat Nil (Unit 1)) (Cat Nil (Cat (Unit 2)(Unit 3)))
eqSeqExample2 = Cat (Cat Nil (Unit 11)) (Cat Nil (Cat (Unit 22)(Unit 33)))
eqSeqExample3 = Cat (Cat Nil (Unit 1)) (Unit 2)



--seq2List: toma una secuencia y devuelve una lisa con los mismos elementos en el mismo orden
--seqList :: Seq e -> [e]
seq2List Nil          = []
seq2List (Unit x)     = [x]
seq2List (Cat s1 s2)  = seq2List s1 ++ seq2List s2

seq2ListExample = Cat (Cat (Cat (Unit 1) (Unit 2)) (Nil)) (Cat (Nil) (Unit 3))

