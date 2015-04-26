main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n = n * (n * n + 5) `div` 6
