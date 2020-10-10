
nextDiv :: Int -> Int -> Int
nextDiv  x y = if y `mod` (x+1) == 0 then (x+1) else nextDiv (x+1) y

isPrime :: Int -> Bool
isPrime n = nextDiv 1 n == n

-- primes: dado un entero n devuelve una lista con los n primeros primos.
primes :: Int -> [Int]
primes n = auxPrime 2 n where
                              auxPrime x 0 = []
                              auxPrime x n = if isPrime x then x:auxPrime (x+1) (n-1) else auxPrime (x+1) n


-- primes': dado un entero n devuelve una lista con los n primeros primos.
primes' :: Int -> [Int]
primes' n = take n (filter isPrime [2..])
