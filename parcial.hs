--examen diciembre

data Bit = O | I deriving Show
data BitT = Nil | Bin Bool BitT BitT deriving Show

contains :: [Bit] -> BitT -> Bool
contains [] Nil = True
contains [] (Bin True Nil Nil) = True
contains (O:bs) (Bin False b1 b2) = contains bs b1
contains (I:bs) (Bin False b1 b2) = contains bs b2
contains _ _ = False


insert:: [Bit] -> BitT -> BitT
insert [] Nil = Bin True Nil Nil
insert [O] (Bin False b1 b2) = Bin False (Bin True Nil Nil) b2
insert [I] (Bin False b1 b2) = Bin False b1 (Bin True Nil Nil)
insert (O:bs) (Bin False b1 b2 ) = Bin False (insert bs b1) b2
insert (I:bs) (Bin False b1 b2 ) = Bin False b1 (insert bs b2)

--intersect :: BitT -> BitT -> BitT


b1 = Bin False (Bin True Nil Nil) (Bin False Nil (Bin True Nil Nil))
b2 = Bin False (Bin True Nil Nil) (Bin False (Bin True Nil Nil) (Bin True Nil Nil))
b3 = Bin False (Bin True Nil Nil) Nil
b4 = Nil

--foldBitT :: (Bool -> a -> a -> a) -> a -> BitT -> a
foldBitT f z1 z2 (Bin True Nil Nil) = z1
foldBitT f z1 z2 Nil = z2
foldBitT f z1 z2 (Bin b b1 b2) = f b (foldBitT f z1 z2 b1) (foldBitT f z1 z2 b2)

type MHash a = a -> [[Bit]]

size :: BitT -> Int
size = foldBitT (\_ r1 r2 -> r1 + r2) 1 0

longestOs = foldBitT (\_ b1 b2 -> 1 + max b1 b2) 1 0
