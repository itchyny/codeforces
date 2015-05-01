main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve (k:as) = length [ a | a <- as, length (filter (`elem`"47") (show a)) <= k ]
solve _ = undefined
