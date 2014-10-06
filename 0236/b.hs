import Data.List (foldl1', group)

main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [a, b, c] = foldl1' sum' [ numberOfDivisors (i * j * k) | i <- [1..a], j <- [1..b], k <- [1..c] ]
  where sum' m n = (m + n) `mod` 1073741824
solve _ = undefined

numberOfDivisors :: Int -> Int
numberOfDivisors = product . map ((+1) . length) . group . primeFactors

primes :: Integral a => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || (n `mod` p /= 0 && r)) True primes

primeFactors :: Integral a => a -> [a]
primeFactors n | n > 1 = go n primes
               | otherwise = []
   where go m ps@(p:t)
          | p * p > m  = [m]
          | r == 0     =  p : go q ps
          | otherwise  =      go m t
             where (q, r) = quotRem m p
         go _ _ = []
