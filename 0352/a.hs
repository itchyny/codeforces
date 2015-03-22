main :: IO ()
main = getContents >>= putStrLn . solve . map read . tail . words

solve :: [Int] -> String
solve as | zero < 1 = "-1"
         | five < 9 = "0"
         | otherwise = replicate (five - five `mod` 9) '5' ++ replicate zero '0'
  where five = length (filter (==5) as)
        zero = length (filter (==0) as)
