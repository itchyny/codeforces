main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Integer] -> Integer
solve as = max (maximum $ zipWith (-) (tail as) as) (minimum $ zipWith (-) (tail $ tail as) as)
