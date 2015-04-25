main :: IO ()
main = readLn >>= putStrLn . unwords . map show . solve

solve :: Integer -> [Integer]
solve n | n < 3 = [ -1 ]
        | otherwise = [n, n - 1 .. 1]
