main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve xs | s `mod` 5 == 0 && s > 0 = s `div` 5
         | otherwise = -1
    where s = sum xs
