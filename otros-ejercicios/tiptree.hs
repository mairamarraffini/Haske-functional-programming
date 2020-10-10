-- #2.
data TipTree a = Tip a | Join (TipTree a) (TipTree a)

heightTip :: TipTree a -> Int
heightTip (Tip a) = 0
heightTip (Join t1 t2) = 1 + (heightTip t1) `max` (heightTip t2)

leaves:: TipTree a -> Int
leaves (Tip a) = 1
leaves (Join t1 t2) = leaves t1 + leaves t2

nodes:: TipTree a -> Int
nodes (Tip a) = 0
nodes (Join t1 t2) = 1 + nodes t1 + nodes t2

walkover:: TipTree a -> [a] 
walkover (Tip a) = [Tip a] 
walkover (Join t1 t2) =  walkover t1 ++ walkover t2

mirrowTip:: TipTree a -> TipTree a
mirrowTip (Tip a) = Tip a
mirrowTip (Join t1 t2) = Join (mirrowTip t1) (mirrowTip t2)

mapTip:: TipTree a -> (a->b) -> TipTree b
mapTip (Tip a) f = Tip (f a)
mapTip (Join t1 t2) f = Join (mapTip t1 f) (mapTip t2 f)

mapTip':: (a->b) -> TipTree a -> TipTree b
mapTip' f (Tip a) = Tip (f a)
mapTip' f (Join t1 t2) = Join (mapTip' f t1) (mapTip' f t2)

treeExample = Join (Tip 1) (Join (Join (Tip 2) (Tip 3)) (Tip 4))


--treeExample
--       JOIN
--    ---------
--    |       |
--   TIP 1   JOIN-
--         --------
--         |       |
--       JOIN    TIP 4
--     -------
--     |      |
--    TIP 2  TIP 3