main :: IO ()
main = getLine >>= mapM_ putStrLn . solve . map read . words

solve :: [Int] -> [String]
solve [n, m] = [ snake k | k <- [0..n-1] ]
  where snake k | even k = replicate m '#'
                | k `mod` 4 == 1 = replicate (m - 1) '.' ++ "#"
                | otherwise = '#' : replicate (m - 1) '.'
solve _ = undefined
