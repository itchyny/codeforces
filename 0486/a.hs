main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n = (if even n then 1 else -1) * (n + 1) `div` 2
