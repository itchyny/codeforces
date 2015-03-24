main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Double] -> Integer
solve xs = round $ 4 * sqrt (product xs) * sum (map (1/) xs)
