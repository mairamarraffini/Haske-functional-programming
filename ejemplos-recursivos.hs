xs = [[1,2,3,4]]
xs1 = [1,2,3]

pairs [n] | mod n 2 == 0 =[n]
          | otherwise    = []

pairs(n:ns) | mod n 2 == 0 = n:pairs(ns)
            | otherwise    = pairs(ns)

squares [n]    = [n*n]
squares (n:ns) = n*n:squares(ns)


remainders [r] n  = [mod r n]
remainders (r:rs) = (mod r n) : remainders rs n 
