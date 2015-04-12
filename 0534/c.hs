main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve (n:a:ds) = [ d - min d (a - n + 1) + max 1 (a - sum ds + d) - 1 | d <- ds ]
solve _ = undefined
