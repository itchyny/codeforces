main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [ n, x, y ] = max 0 $ (n * y + 99) `div` 100 - x
solve _ = undefined
