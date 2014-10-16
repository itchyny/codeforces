main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve rgb = go (maximum rgb) (sum rgb - maximum rgb - minimum rgb) (minimum rgb)
  where go r g b | r >= 2 * (g + b) = g + b
                 | otherwise = (r + g + b) `div` 3
