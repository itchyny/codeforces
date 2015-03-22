main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . tail . words

solve :: [Int] -> [Int]
solve = go (0, 0, True)
  where go (a, b, _) [] = [a, b]
        go (a, b, c) as | c && head as > last as = go (a + head as, b, not c) (tail as)
                        |      head as > last as = go (a, b + head as, not c) (tail as)
                        | c                      = go (a + last as, b, not c) (init as)
                        | otherwise              = go (a, b + last as, not c) (init as)
