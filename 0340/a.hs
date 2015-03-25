main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [ x, y, a, b ] = b `div` (x `lcm` y) - (a - 1) `div` (x `lcm` y)
solve _ = undefined
