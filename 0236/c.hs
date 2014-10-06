main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n = maximum [ a `lcm` b `lcm` c | a <- [n,n-1..p],b <- [a,a-1..p],c <- [b,b-1..p] ]
  where p = last (take 3 $ take 3 (filter isPrime [n,n-1..1]) ++ [1])

primes :: Integral a => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || (n `mod` p /= 0 && r)) True primes
