main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [ n, m ] = last (takeWhile (>=0) (scanl (-) m (cycle [1..n])))
solve _ = undefined
