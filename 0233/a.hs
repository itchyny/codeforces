main :: IO ()
main = readLn >>= putStrLn . unwords . map show . solve

solve :: Integer -> [Integer]
solve n | even n = concatMap (\i -> [ 2 * i, 2 * i - 1 ]) [ 1 .. n `div` 2 ]
        | otherwise = [ -1 ]
