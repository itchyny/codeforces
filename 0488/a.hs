main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n = head (filter (elem '8' . show) [n + 1 .. ]) - n
