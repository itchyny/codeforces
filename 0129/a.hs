main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve as = length [ a | a <- as, odd a == odd (sum as) ]
