main :: IO ()
main = readLn >>= putStrLn . solve

solve :: Int -> String
solve n | n >= 4 && n `mod` 2 == 0 = "YES"
        | otherwise = "NO"
