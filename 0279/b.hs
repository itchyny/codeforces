main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (_:t:as) = go 0 0 0 as as
  where go a k s xxs@(x:xs) (y:ys) | s + y <= t = go (max a (k + 1)) (k + 1) (s + y) xxs ys
                                   | otherwise = go a k (s + y - x) xs ys
        go a _ _ _ _ = a
solve _ = undefined
