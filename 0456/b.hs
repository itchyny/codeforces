main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n | n `mod` 4 == 0 = 4
        | otherwise = 0
