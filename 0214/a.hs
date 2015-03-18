main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [ n, m ] = length [ (a, b) | a <- [ 0 .. floor (sqrt (fromIntegral n) :: Double) ], let b = n - a * a, a + b * b == m ]
solve _ = undefined
