--evenPos :: [a] -> [a]
evenPos = foldr [] (\x xs -> if not (null xs) then x:tail xs else [x])
