main :: IO ()
main = readLn >>= putStrLn . unwords . map show . solve

solve :: Int -> [Int]
solve n | even n = [ 4, n - 4 ]
        | otherwise = [ 9, n - 9 ]
