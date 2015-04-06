main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve (_:m:as) = snd $ foldl f (0, 1) as
  where f (x, y) a | x + a <= m = (x + a, y)
                   | otherwise = (a, y + 1)
solve _ = undefined
