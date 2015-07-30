import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = solve <$> readLn <*> (map read . words <$> getContents) >>= print

solve :: Integer -> [Integer] -> Integer
solve n xs = sum [ 1 | i <- [1..5], (sum xs + i) `mod` (n + 1) /= 1 ]
