main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n = 3 * n `div` 2
