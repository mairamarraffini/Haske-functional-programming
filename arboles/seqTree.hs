-- #4.

data Seq e = Nil | Unit e | Cat (Seq e) (Seq e) deriving Show

seqExample = Cat (Cat Nil (Unit 1)) (Cat (Unit 2) Nil)

--appSeq: toma dos secuencias y devuelve su concatenación
appSeq :: Seq e -> Seq e -> Seq e
appSeq s1 s2  = Cat s1 s2

appSeqEx1 = Cat Nil (Unit 1)
appSeqEx2 = Nil
appSeqEx3 = Cat (Unit 2) (Cat Nil (Unit 3))

--conSeq: toma un elemento y una secuencia y devuelve la secuencia que tiene al elemento dado como cabeza y a la secuencia dada como cola
conSeq :: a -> Seq a -> Seq a
conSeq x seq = Cat (Unit x) seq

conSeqEx1 = Cat Nil Nil
conSeqEx2 = Cat (Unit 2) (Cat Nil (Unit 3))
conSeqEx3 = Unit 1

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
headSeqAux :: Seq e -> Maybe e

headSeqAux Nil        = Nothing
headSeqAux (Unit x)   = Just x
headSeqAux Cat s1 s2 = if isNothing (headSeqAux s1) then s2 else fromJust (headSeqAux s1)
--headSeqAux Cat s1 s2  = let 
--                           r1 = headSeqAux s1
--                           r2 = headSeqAux s2
--                        in
--                            if isNothing r1 then r2
--                             else fromJust r1



--headSeq Nil          = 
--headSeq (Unit x)     = x
--headSeq (Cat Nil s2) = headSeq s2
--headSeq (Cat s1 Nil) = headSeq s1
--headSeq (Cat s1 s2)  = 


--conSeq: toma un elemento y una secuencia y devuelve la secuencia que tiene al elemento dado como cabeza y a la secuencia dada como cola.

--normSeq: elimina todos los Nil innecesarios de una secuencia.
--normSeq :: Seq e -> Seq e
normSeq Nil = Nil
normSeq (Unit x) = Unit x
normSeq (Cat Nil Nil) = Nil
normSeq (Cat s1 Nil)  = normSeq s1
normSeq (Cat Nil s2)  = normSeq s2
normseq (Cat s1 s2)   = Cat (normSeq s1) (normSeq s2)

normSeqExample = Cat (Cat Nil (Unit 1)) Nil

--eqSeq: toma dos secuencias y devuelve True sii ambas contienen los mismos valores, en el mismo orden y en la misma cantidad
--eqSeq :: Seq e -> Seq e -> Bool
eqSeq Nil Nil = True
eqSeq (Unit x) (Unit y) = x==y
eqSeq Nil s2 = False
eqSeq s1 Nil = False
eqSeq (Unit x) s2 = False
eqSeq s1 (Unit x) = False
eqSeq (Cat s1 s2) (Cat s3 s4) = (eqSeq s1 s3) && (eqSeq s2 s4)

eqSeqExample1 = Cat (Cat Nil (Unit 1)) (Cat Nil (Cat (Unit 2)(Unit 3)))
eqSeqExample2 = Cat (Cat Nil (Unit 11)) (Cat Nil (Cat (Unit 22)(Unit 33)))
eqSeqExample3 = Cat (Cat Nil (Unit 1)) (Unit 2)



--seq2List: toma una secuencia y devuelve una lisa con los mismos elementos en el mismo orden
--seqList :: Seq e -> [e]
seq2List Nil          = []
seq2List (Unit x)     = [x]
seq2List (Cat s1 s2)  = seq2List s1 ++ seq2List s2

seq2ListExample = Cat (Cat (Cat (Unit 1) (Unit 2)) (Nil)) (Cat (Nil) (Unit 3))


