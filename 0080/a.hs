import Control.Applicative ((<$>))

main :: IO ()
main = putStrLn . yesno =<< solve <$> (toTuple . map read . words <$> getLine)

solve :: (Integer, Integer) -> Bool
solve (n, m) = m == head (filter isPrime [n+1..])

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"

primes :: Integral a => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || n `mod` p /= 0 && r) True primes
