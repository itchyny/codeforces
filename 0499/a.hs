main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Integer] -> Integer
solve (x:xs) = go 0 xs
  where go t zs@(l:r:ys) | t + x < l = go (t + x) zs
                         | otherwise = r - t + go r ys
        go _ _ = 0
solve _ = undefined
