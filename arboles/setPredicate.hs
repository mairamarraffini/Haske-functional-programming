-- #1.b

data Set e = Set (e->Bool) 
instance Show (Set e)  where 
  show (Set e) = "True baby" --no se  que se define acá

-- belongs :: e -> Set e -> Bool
belongs x (Set p) = p x

-- union :: Set b -> Set b -> Set b
union (Set p1) (Set p2) = Set (\e-> p1 e && p2 e)


--intersection :: 
