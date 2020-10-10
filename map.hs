--map: esquema de trabajo sobre listas como funcioones de alto orden

--definida por recursión sobre la lista
succl []     = []
succl (n:ns) = (n+1) : succl ns

--definida con lambda 
succl1 []     = []
succl1 (n:ns) = (\n'->n'+1) n : succl1 (n:ns)

--definida con map
succlMap :: [Integer] -> [Integer]
succlMap = map (\n'->n'+1)
