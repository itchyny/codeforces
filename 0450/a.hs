main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (_:m:as) = snd $ maximum [ ((a + m - 1) `div` m, i) | (i, a) <- zip [1..] as ]
solve _ = undefined
