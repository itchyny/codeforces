main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Double] -> Double
solve (n:ps) = sum ps / n
solve _ = undefined
