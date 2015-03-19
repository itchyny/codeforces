main :: IO ()
main = readLn >>= mapM_ putStrLn . solve

solve :: Int -> [String]
solve n = show ((n * n + 1) `div` 2) : take n (cycle [ take n $ cycle "C.", take n $ cycle ".C" ])
