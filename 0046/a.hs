import Control.Applicative ((<$>))

main :: IO ()
main = putStrLn <$> unwords <$> map show =<< solve <$> readLn

solve :: Integer -> [Integer]
solve n = [ (i * (i + 1) `div` 2) `mod` n + 1 | i <- [1..n-1] ]
