main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve [n, k, _, _, sall, sk] = f sk k ++ f (sall - sk) (n - k)
  where f _ 0 = []
        f s m = replicate (s `mod` m) ((s + m - 1) `div` m)
             ++ replicate (m - s `mod` m) (s `div` m)
solve _ = undefined
