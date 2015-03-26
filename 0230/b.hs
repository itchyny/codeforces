main :: IO ()
main = getContents >>= mapM_ (putStrLn . yesno . solve . read) . tail . words

solve :: Integer -> Bool
solve n = n == m * m && isPrime m
  where m = round (sqrt (fromIntegral n) :: Double)

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"

primes :: Integral a => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || n `mod` p /= 0 && r) True primes
