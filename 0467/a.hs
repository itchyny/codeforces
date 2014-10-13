main :: IO ()
main = getContents >>= print . solve . map (map read . words) . tail . lines

solve :: [[Int]] -> Int
solve pqs = length [ () | [p, q] <- pqs, p + 2 <= q ]
