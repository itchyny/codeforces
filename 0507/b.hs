main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Double] -> Integer
solve [r, x, y, x', y'] = ceiling (sqrt ((x - x') * (x - x') + (y - y') * (y - y')) / (2 * r))
solve _ = undefined
