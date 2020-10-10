-- #1.a.

--Set :: [e] -> Set e
data Set x = Set [x] deriving Show

mkSet :: [a] -> Set a
mkSet xs = Set $ removeDups xs

naturals :: Set Int
naturals = Set [1..]

--belongs':: e -> Set e -> Bool
belongs' x (Set xs) = belongs x xs

--belongss :: e -> [e] -> Bool
belongs x []     = False
belongs x (y:ys) = x==y || (belongs x ys)

--intersection :: Set e -> Set e -> Set e
intersection (Set xs) (Set ys) = Set (intersection' xs ys)

--intersection' :: [e] -> [e] -> [e]
intersection' xs []     = []
intersection' [] ys     = []
intersection' xs ys = intersectionAux xs ys [] 
    where intersectionAux [] ys zs     = zs
          intersectionAux (x:xs) ys zs = intersectionAux xs ys (appendIfBelongs x ys zs)

-- appendIfBelongs :: e -> [e] -> [ys]
appendIfBelongs x xs ys | x `belongs` xs = ys ++ [x]
                        | otherwise      = ys

--union :: Set e -> Set e -> Set e
union (Set xs) (Set ys) = Set zs  where zs = union' xs ys  

-- union' :: [e] -> [e] -> [e]
union' xs [] = xs
union' [] ys = ys
union' (x:xs) ys = union' xs (appendIfNotBelongs x ys)

-- appendIfNotBelongs :: e -> [e] -> [e]
appendIfNotBelongs x ys | x `belongs` ys  = ys
                        | otherwise       = ys ++ [x]

union'' (Set xs) (Set ys) = Set $ removeDups $ xs ++ ys