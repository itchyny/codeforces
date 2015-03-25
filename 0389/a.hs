main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve xs = length xs * foldr1 gcd xs
