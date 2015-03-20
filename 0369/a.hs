import Data.List (foldl')

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (_:m:k:as) = snd $ foldl' f ((m, k), 0) as
  where f ((b, c), d) 1 | b > 0 = ((b - 1, c), d)
                        | otherwise = ((b, c), d + 1)
        f ((b, c), d) _ | c > 0 = ((b, c - 1), d)
                        | b > 0 = ((b - 1, c), d)
                        | otherwise = ((b, c), d + 1)
solve _ = undefined
